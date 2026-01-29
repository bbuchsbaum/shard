//! Share planning for deep sharing transparency
//!
//! This module provides `share_plan()` which analyzes an object structure and returns
//! a detailed plan of what would be shared, kept, aliased, or errored - without
//! actually performing the sharing.
//!
//! # Purpose
//!
//! Before sharing complex nested structures (lists, data frames), users want to know:
//! - Which components will be shared?
//! - Which will be kept in place?
//! - Are there any issues (cycles, unsupported types)?
//!
//! `share_plan()` provides this transparency without side effects.
//!
//! # Example
//!
//! ```
//! use shard::share_plan::{share_plan, SharePlanConfig, ValueInfo, ValueType};
//!
//! // Analyze a simple structure with low threshold
//! let config = SharePlanConfig::default().with_min_bytes(1000);
//! let root = ValueInfo::new_atomic(ValueType::Double, 10000, 80000);
//! let plan = share_plan(&root, &config);
//!
//! // Check what would happen
//! assert_eq!(plan.summary().share_count, 1);
//! println!("{}", plan);
//! ```

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Action to take for a node in the share plan
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShareAction {
    /// Share this value (create shared segment)
    Share,
    /// Keep this value as-is (don't share)
    Keep,
    /// This is an alias of another node (will point to same shared segment)
    Alias,
    /// Error - cannot proceed with this value
    Error,
}

impl fmt::Display for ShareAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ShareAction::Share => write!(f, "share"),
            ShareAction::Keep => write!(f, "keep"),
            ShareAction::Alias => write!(f, "alias"),
            ShareAction::Error => write!(f, "error"),
        }
    }
}

/// Reason for the action chosen
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShareReason {
    // Share reasons
    /// Large enough to be worth sharing
    EligibleLargeEnough,
    /// Forced to share by hook/configuration
    EligibleForcedByHook,

    // Keep reasons
    /// Below minimum byte threshold
    BelowMinBytes,
    /// Type not enabled for sharing
    TypeNotEnabled,
    /// Non-atomic leaf (e.g., function, environment)
    NonAtomicLeaf,
    /// Contains non-shareable reference
    NonShareableReference,
    /// Exceeded maximum traversal depth
    MaxDepthExceeded,
    /// Cycle detected in structure
    CycleDetected,
    /// Skipped by hook
    HookSkip,

    // Error reasons
    /// Strict mode doesn't allow this non-shareable type
    StrictNonShareable,
    /// Cycle detected and cycle policy is 'error'
    CycleError,

    // Alias reason
    /// This is an alias of another node
    AliasOf(usize),
}

impl fmt::Display for ShareReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ShareReason::EligibleLargeEnough => write!(f, "eligible_large_enough"),
            ShareReason::EligibleForcedByHook => write!(f, "eligible_forced_by_hook"),
            ShareReason::BelowMinBytes => write!(f, "below_min_bytes"),
            ShareReason::TypeNotEnabled => write!(f, "type_not_enabled"),
            ShareReason::NonAtomicLeaf => write!(f, "non_atomic_leaf"),
            ShareReason::NonShareableReference => write!(f, "non_shareable_reference"),
            ShareReason::MaxDepthExceeded => write!(f, "max_depth_exceeded"),
            ShareReason::CycleDetected => write!(f, "cycle_detected"),
            ShareReason::HookSkip => write!(f, "hook_skip"),
            ShareReason::StrictNonShareable => write!(f, "strict_non_shareable"),
            ShareReason::CycleError => write!(f, "cycle_error"),
            ShareReason::AliasOf(id) => write!(f, "alias_of:{}", id),
        }
    }
}

/// Value types that can be shared
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueType {
    Double,
    Integer,
    Logical,
    Raw,
    Complex,
    Character,
    List,
    DataFrame,
    Environment,
    Function,
    Other,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueType::Double => write!(f, "double"),
            ValueType::Integer => write!(f, "integer"),
            ValueType::Logical => write!(f, "logical"),
            ValueType::Raw => write!(f, "raw"),
            ValueType::Complex => write!(f, "complex"),
            ValueType::Character => write!(f, "character"),
            ValueType::List => write!(f, "list"),
            ValueType::DataFrame => write!(f, "data.frame"),
            ValueType::Environment => write!(f, "environment"),
            ValueType::Function => write!(f, "function"),
            ValueType::Other => write!(f, "other"),
        }
    }
}

impl ValueType {
    /// Check if this type is atomic (can be directly shared)
    pub fn is_atomic(&self) -> bool {
        matches!(
            self,
            ValueType::Double
                | ValueType::Integer
                | ValueType::Logical
                | ValueType::Raw
                | ValueType::Complex
        )
    }

    /// Check if this type is a container (can have children)
    pub fn is_container(&self) -> bool {
        matches!(
            self,
            ValueType::List | ValueType::DataFrame | ValueType::Environment
        )
    }
}

/// Information about a value to be analyzed
#[derive(Debug, Clone)]
pub struct ValueInfo {
    /// Type of the value
    pub value_type: ValueType,
    /// Length (number of elements)
    pub length: usize,
    /// Estimated size in bytes
    pub bytes_est: usize,
    /// Dimensions (for matrices/arrays)
    pub dim: Option<Vec<usize>>,
    /// Class attribute (for S3/S4 objects)
    pub class: Option<Vec<String>>,
    /// Children (for containers)
    pub children: Vec<(String, ValueInfo)>,
    /// Unique identity for alias detection (memory address or similar)
    pub identity: Option<usize>,
}

impl ValueInfo {
    /// Create a new atomic value info
    pub fn new_atomic(value_type: ValueType, length: usize, bytes_est: usize) -> Self {
        Self {
            value_type,
            length,
            bytes_est,
            dim: None,
            class: None,
            children: Vec::new(),
            identity: None,
        }
    }

    /// Create a new container value info
    pub fn new_container(value_type: ValueType, children: Vec<(String, ValueInfo)>) -> Self {
        let bytes_est = children.iter().map(|(_, c)| c.bytes_est).sum();
        Self {
            value_type,
            length: children.len(),
            bytes_est,
            dim: None,
            class: None,
            children,
            identity: None,
        }
    }

    /// Set dimensions
    pub fn with_dim(mut self, dim: Vec<usize>) -> Self {
        self.dim = Some(dim);
        self
    }

    /// Set class
    pub fn with_class(mut self, class: Vec<String>) -> Self {
        self.class = Some(class);
        self
    }

    /// Set identity for alias detection
    pub fn with_identity(mut self, identity: usize) -> Self {
        self.identity = Some(identity);
        self
    }
}

/// Sharing mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ShareMode {
    /// Strict mode: error on non-shareable types
    Strict,
    /// Balanced mode: keep non-shareable types in place
    #[default]
    Balanced,
}

/// Cycle handling policy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CyclePolicy {
    /// Error when cycle detected
    Error,
    /// Skip cyclic references
    #[default]
    Skip,
}

/// Configuration for share planning
#[derive(Debug, Clone)]
pub struct SharePlanConfig {
    /// Whether to recursively analyze containers
    pub deep: bool,
    /// Sharing mode (strict or balanced)
    pub mode: ShareMode,
    /// Minimum bytes to consider for sharing
    pub min_bytes: usize,
    /// Types enabled for sharing
    pub enabled_types: HashSet<ValueType>,
    /// Maximum depth for recursive analysis
    pub max_depth: usize,
    /// Whether to preserve aliases
    pub preserve_aliases: bool,
    /// How to handle cycles
    pub cycle_policy: CyclePolicy,
}

impl Default for SharePlanConfig {
    fn default() -> Self {
        let mut enabled_types = HashSet::new();
        enabled_types.insert(ValueType::Double);
        enabled_types.insert(ValueType::Integer);
        enabled_types.insert(ValueType::Logical);
        enabled_types.insert(ValueType::Raw);
        enabled_types.insert(ValueType::Complex);

        Self {
            deep: false,
            mode: ShareMode::Balanced,
            min_bytes: 64 * 1024 * 1024, // 64MB default
            enabled_types,
            max_depth: usize::MAX,
            preserve_aliases: true,
            cycle_policy: CyclePolicy::Skip,
        }
    }
}

impl SharePlanConfig {
    /// Create a config for deep sharing
    pub fn deep() -> Self {
        Self {
            deep: true,
            ..Default::default()
        }
    }

    /// Set minimum bytes threshold
    pub fn with_min_bytes(mut self, min_bytes: usize) -> Self {
        self.min_bytes = min_bytes;
        self
    }

    /// Set sharing mode
    pub fn with_mode(mut self, mode: ShareMode) -> Self {
        self.mode = mode;
        self
    }

    /// Set maximum depth
    pub fn with_max_depth(mut self, max_depth: usize) -> Self {
        self.max_depth = max_depth;
        self
    }

    /// Set cycle policy
    pub fn with_cycle_policy(mut self, policy: CyclePolicy) -> Self {
        self.cycle_policy = policy;
        self
    }
}

/// A node in the share plan
#[derive(Debug, Clone)]
pub struct SharePlanNode {
    /// Unique node ID
    pub node_id: usize,
    /// Parent node ID (None for root)
    pub parent_id: Option<usize>,
    /// Path to this node (e.g., "$data[[1]]")
    pub path: String,
    /// Whether this is a container
    pub is_container: bool,
    /// R type
    pub r_type: ValueType,
    /// Class attribute
    pub class: Option<Vec<String>>,
    /// Dimensions
    pub dim: Option<Vec<usize>>,
    /// Length
    pub length: usize,
    /// Estimated bytes
    pub bytes_est: usize,
    /// Whether this type is shareable
    pub shareable: bool,
    /// Action to take
    pub action: ShareAction,
    /// Reason for action
    pub reason: ShareReason,
    /// If alias, which node it's an alias of
    pub alias_of: Option<usize>,
}

/// Summary of share plan actions
#[derive(Debug, Clone, Default)]
pub struct SharePlanSummary {
    /// Number of nodes to share
    pub share_count: usize,
    /// Number of nodes to keep
    pub keep_count: usize,
    /// Number of alias nodes
    pub alias_count: usize,
    /// Number of error nodes
    pub error_count: usize,
    /// Total bytes to be shared
    pub total_share_bytes: usize,
    /// Total bytes kept
    pub total_keep_bytes: usize,
}

/// A complete share plan
#[derive(Debug, Clone)]
pub struct SharePlan {
    /// All nodes in the plan
    nodes: Vec<SharePlanNode>,
    /// Summary statistics
    summary: SharePlanSummary,
}

impl SharePlan {
    /// Get all nodes in the plan
    pub fn nodes(&self) -> &[SharePlanNode] {
        &self.nodes
    }

    /// Get the summary
    pub fn summary(&self) -> &SharePlanSummary {
        &self.summary
    }

    /// Get nodes by action
    pub fn nodes_by_action(&self, action: ShareAction) -> Vec<&SharePlanNode> {
        self.nodes.iter().filter(|n| n.action == action).collect()
    }

    /// Check if the plan has any errors
    pub fn has_errors(&self) -> bool {
        self.summary.error_count > 0
    }

    /// Get the root node
    pub fn root(&self) -> Option<&SharePlanNode> {
        self.nodes.first()
    }
}

impl fmt::Display for SharePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Share Plan")?;
        writeln!(f, "==========")?;
        writeln!(f)?;

        // Summary
        writeln!(f, "Summary:")?;
        writeln!(f, "  Share: {} nodes ({} bytes)", self.summary.share_count, self.summary.total_share_bytes)?;
        writeln!(f, "  Keep:  {} nodes ({} bytes)", self.summary.keep_count, self.summary.total_keep_bytes)?;
        writeln!(f, "  Alias: {} nodes", self.summary.alias_count)?;
        if self.summary.error_count > 0 {
            writeln!(f, "  Error: {} nodes", self.summary.error_count)?;
        }
        writeln!(f)?;

        // Table
        writeln!(f, "Nodes:")?;
        writeln!(f, "{:<4} {:<20} {:<10} {:<10} {:<12} Reason",
                 "ID", "Path", "Type", "Action", "Bytes")?;
        writeln!(f, "{:-<70}", "")?;

        for node in &self.nodes {
            let path = if node.path.len() > 18 {
                format!("...{}", &node.path[node.path.len()-15..])
            } else {
                node.path.clone()
            };
            writeln!(f, "{:<4} {:<20} {:<10} {:<10} {:<12} {}",
                     node.node_id,
                     path,
                     node.r_type,
                     node.action,
                     node.bytes_est,
                     node.reason)?;
        }

        Ok(())
    }
}

/// Internal state for share planning
struct PlanBuilder {
    nodes: Vec<SharePlanNode>,
    next_id: usize,
    seen_identities: HashMap<usize, usize>, // identity -> node_id
    in_progress: HashSet<usize>, // identities currently being visited (cycle detection)
}

impl PlanBuilder {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            next_id: 0,
            seen_identities: HashMap::new(),
            in_progress: HashSet::new(),
        }
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn build(self) -> SharePlan {
        let mut summary = SharePlanSummary::default();

        for node in &self.nodes {
            match node.action {
                ShareAction::Share => {
                    summary.share_count += 1;
                    summary.total_share_bytes += node.bytes_est;
                }
                ShareAction::Keep => {
                    summary.keep_count += 1;
                    summary.total_keep_bytes += node.bytes_est;
                }
                ShareAction::Alias => {
                    summary.alias_count += 1;
                }
                ShareAction::Error => {
                    summary.error_count += 1;
                }
            }
        }

        SharePlan {
            nodes: self.nodes,
            summary,
        }
    }
}

/// Analyze a value and create a share plan
fn analyze_value(
    builder: &mut PlanBuilder,
    value: &ValueInfo,
    path: String,
    parent_id: Option<usize>,
    depth: usize,
    config: &SharePlanConfig,
) {
    let node_id = builder.next_node_id();

    // Check for alias (same identity seen before)
    if let Some(identity) = value.identity {
        // Check for cycle (currently visiting this identity)
        if builder.in_progress.contains(&identity) {
            let (action, reason) = match config.cycle_policy {
                CyclePolicy::Error => (ShareAction::Error, ShareReason::CycleError),
                CyclePolicy::Skip => (ShareAction::Keep, ShareReason::CycleDetected),
            };

            builder.nodes.push(SharePlanNode {
                node_id,
                parent_id,
                path,
                is_container: value.value_type.is_container(),
                r_type: value.value_type,
                class: value.class.clone(),
                dim: value.dim.clone(),
                length: value.length,
                bytes_est: value.bytes_est,
                shareable: false,
                action,
                reason,
                alias_of: None,
            });
            return;
        }

        // Check for alias (seen before but not in current path)
        if config.preserve_aliases {
            if let Some(&original_id) = builder.seen_identities.get(&identity) {
                builder.nodes.push(SharePlanNode {
                    node_id,
                    parent_id,
                    path,
                    is_container: value.value_type.is_container(),
                    r_type: value.value_type,
                    class: value.class.clone(),
                    dim: value.dim.clone(),
                    length: value.length,
                    bytes_est: 0, // Alias doesn't count towards bytes
                    shareable: true,
                    action: ShareAction::Alias,
                    reason: ShareReason::AliasOf(original_id),
                    alias_of: Some(original_id),
                });
                return;
            }
        }

        // Record this identity
        builder.seen_identities.insert(identity, node_id);
        builder.in_progress.insert(identity);
    }

    // Determine action and reason
    let (action, reason) = determine_action(value, depth, config);

    let shareable = value.value_type.is_atomic() && config.enabled_types.contains(&value.value_type);

    builder.nodes.push(SharePlanNode {
        node_id,
        parent_id,
        path: path.clone(),
        is_container: value.value_type.is_container(),
        r_type: value.value_type,
        class: value.class.clone(),
        dim: value.dim.clone(),
        length: value.length,
        bytes_est: value.bytes_est,
        shareable,
        action,
        reason,
        alias_of: None,
    });

    // Recursively analyze children if deep mode and this is a container
    if config.deep && value.value_type.is_container() && depth < config.max_depth {
        for (name, child) in &value.children {
            let child_path = if name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                format!("{}${}", path, name)
            } else {
                format!("{}[[\"{}\"]]", path, name)
            };
            analyze_value(builder, child, child_path, Some(node_id), depth + 1, config);
        }
    }

    // Remove from in_progress after visiting children
    if let Some(identity) = value.identity {
        builder.in_progress.remove(&identity);
    }
}

/// Determine the action and reason for a value
fn determine_action(value: &ValueInfo, depth: usize, config: &SharePlanConfig) -> (ShareAction, ShareReason) {
    // Check depth limit
    if depth >= config.max_depth {
        return (ShareAction::Keep, ShareReason::MaxDepthExceeded);
    }

    // Non-shareable types
    if matches!(value.value_type, ValueType::Function | ValueType::Environment) {
        return match config.mode {
            ShareMode::Strict => (ShareAction::Error, ShareReason::StrictNonShareable),
            ShareMode::Balanced => (ShareAction::Keep, ShareReason::NonShareableReference),
        };
    }

    // Containers are not directly shared (their children might be)
    if value.value_type.is_container() {
        return (ShareAction::Keep, ShareReason::NonAtomicLeaf);
    }

    // Check if type is enabled
    if !config.enabled_types.contains(&value.value_type) {
        return (ShareAction::Keep, ShareReason::TypeNotEnabled);
    }

    // Check size threshold
    if value.bytes_est < config.min_bytes {
        return (ShareAction::Keep, ShareReason::BelowMinBytes);
    }

    // Eligible for sharing
    (ShareAction::Share, ShareReason::EligibleLargeEnough)
}

/// Create a share plan for a value
///
/// Analyzes the value structure and returns a detailed plan showing what would
/// be shared, kept, aliased, or errored.
///
/// # Arguments
///
/// * `value` - Information about the value to analyze
/// * `config` - Configuration for the analysis
///
/// # Returns
///
/// A `SharePlan` containing detailed information about each node
pub fn share_plan(value: &ValueInfo, config: &SharePlanConfig) -> SharePlan {
    let mut builder = PlanBuilder::new();
    analyze_value(&mut builder, value, "<root>".to_string(), None, 0, config);
    builder.build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_large_matrix() {
        let config = SharePlanConfig::default().with_min_bytes(1000);
        let value = ValueInfo::new_atomic(ValueType::Double, 10000, 80000)
            .with_dim(vec![100, 100]);

        let plan = share_plan(&value, &config);

        assert_eq!(plan.summary().share_count, 1);
        assert_eq!(plan.summary().keep_count, 0);
        assert_eq!(plan.nodes()[0].action, ShareAction::Share);
        assert_eq!(plan.nodes()[0].reason, ShareReason::EligibleLargeEnough);
    }

    #[test]
    fn test_small_vector_kept() {
        let config = SharePlanConfig::default(); // 64MB default threshold
        let value = ValueInfo::new_atomic(ValueType::Double, 100, 800);

        let plan = share_plan(&value, &config);

        assert_eq!(plan.summary().keep_count, 1);
        assert_eq!(plan.nodes()[0].action, ShareAction::Keep);
        assert_eq!(plan.nodes()[0].reason, ShareReason::BelowMinBytes);
    }

    #[test]
    fn test_deep_list_analysis() {
        let config = SharePlanConfig::deep().with_min_bytes(1000);

        let child1 = ValueInfo::new_atomic(ValueType::Double, 10000, 80000);
        let child2 = ValueInfo::new_atomic(ValueType::Integer, 100, 400);
        let list = ValueInfo::new_container(
            ValueType::List,
            vec![
                ("big".to_string(), child1),
                ("small".to_string(), child2),
            ],
        );

        let plan = share_plan(&list, &config);

        // List itself kept, big child shared, small child kept
        assert_eq!(plan.summary().share_count, 1);
        assert_eq!(plan.summary().keep_count, 2);

        // Find nodes by path
        let root = plan.nodes().iter().find(|n| n.path == "<root>").unwrap();
        assert_eq!(root.action, ShareAction::Keep);

        let big = plan.nodes().iter().find(|n| n.path == "<root>$big").unwrap();
        assert_eq!(big.action, ShareAction::Share);

        let small = plan.nodes().iter().find(|n| n.path == "<root>$small").unwrap();
        assert_eq!(small.action, ShareAction::Keep);
    }

    #[test]
    fn test_type_not_enabled() {
        let mut config = SharePlanConfig::default().with_min_bytes(100);
        config.enabled_types.remove(&ValueType::Integer);

        let value = ValueInfo::new_atomic(ValueType::Integer, 10000, 40000);

        let plan = share_plan(&value, &config);

        assert_eq!(plan.nodes()[0].action, ShareAction::Keep);
        assert_eq!(plan.nodes()[0].reason, ShareReason::TypeNotEnabled);
    }

    #[test]
    fn test_environment_strict_mode() {
        let config = SharePlanConfig::default().with_mode(ShareMode::Strict);
        let value = ValueInfo::new_atomic(ValueType::Environment, 0, 0);

        let plan = share_plan(&value, &config);

        assert_eq!(plan.summary().error_count, 1);
        assert_eq!(plan.nodes()[0].action, ShareAction::Error);
        assert_eq!(plan.nodes()[0].reason, ShareReason::StrictNonShareable);
    }

    #[test]
    fn test_environment_balanced_mode() {
        let config = SharePlanConfig::default().with_mode(ShareMode::Balanced);
        let value = ValueInfo::new_atomic(ValueType::Environment, 0, 0);

        let plan = share_plan(&value, &config);

        assert_eq!(plan.summary().keep_count, 1);
        assert_eq!(plan.nodes()[0].action, ShareAction::Keep);
        assert_eq!(plan.nodes()[0].reason, ShareReason::NonShareableReference);
    }

    #[test]
    fn test_alias_detection() {
        let config = SharePlanConfig::deep().with_min_bytes(100);

        // Create two children with same identity (aliased)
        let child = ValueInfo::new_atomic(ValueType::Double, 1000, 8000)
            .with_identity(12345);

        let list = ValueInfo::new_container(
            ValueType::List,
            vec![
                ("first".to_string(), child.clone()),
                ("second".to_string(), child), // Same identity
            ],
        );

        let plan = share_plan(&list, &config);

        // One shared, one alias
        assert_eq!(plan.summary().share_count, 1);
        assert_eq!(plan.summary().alias_count, 1);

        let second = plan.nodes().iter().find(|n| n.path == "<root>$second").unwrap();
        assert_eq!(second.action, ShareAction::Alias);
    }

    #[test]
    fn test_cycle_skip() {
        let config = SharePlanConfig::deep()
            .with_min_bytes(100)
            .with_cycle_policy(CyclePolicy::Skip);

        // Simulate a cycle by having a node reference itself
        let mut cyclic = ValueInfo::new_container(ValueType::List, vec![])
            .with_identity(99999);

        // Add a child that references back (same identity)
        cyclic.children.push((
            "self_ref".to_string(),
            ValueInfo::new_atomic(ValueType::List, 0, 0).with_identity(99999),
        ));

        let plan = share_plan(&cyclic, &config);

        // Should have cycle detected, kept
        let cycle_node = plan.nodes().iter().find(|n| n.path == "<root>$self_ref").unwrap();
        assert_eq!(cycle_node.action, ShareAction::Keep);
        assert_eq!(cycle_node.reason, ShareReason::CycleDetected);
    }

    #[test]
    fn test_cycle_error() {
        let config = SharePlanConfig::deep()
            .with_min_bytes(100)
            .with_cycle_policy(CyclePolicy::Error);

        let mut cyclic = ValueInfo::new_container(ValueType::List, vec![])
            .with_identity(99999);

        cyclic.children.push((
            "self_ref".to_string(),
            ValueInfo::new_atomic(ValueType::List, 0, 0).with_identity(99999),
        ));

        let plan = share_plan(&cyclic, &config);

        assert!(plan.has_errors());
        let cycle_node = plan.nodes().iter().find(|n| n.path == "<root>$self_ref").unwrap();
        assert_eq!(cycle_node.action, ShareAction::Error);
        assert_eq!(cycle_node.reason, ShareReason::CycleError);
    }

    #[test]
    fn test_max_depth() {
        let config = SharePlanConfig::deep()
            .with_min_bytes(100)
            .with_max_depth(1);

        let deep = ValueInfo::new_container(
            ValueType::List,
            vec![(
                "level1".to_string(),
                ValueInfo::new_container(
                    ValueType::List,
                    vec![(
                        "level2".to_string(),
                        ValueInfo::new_atomic(ValueType::Double, 1000, 8000),
                    )],
                ),
            )],
        );

        let plan = share_plan(&deep, &config);

        // Level 2 should not be analyzed (max_depth=1 means only root and direct children)
        // Actually with depth starting at 0, max_depth=1 means we stop at depth 1
        let level1 = plan.nodes().iter().find(|n| n.path == "<root>$level1");
        assert!(level1.is_some());
    }

    #[test]
    fn test_plan_display() {
        let config = SharePlanConfig::default().with_min_bytes(1000);
        let value = ValueInfo::new_atomic(ValueType::Double, 10000, 80000);

        let plan = share_plan(&value, &config);
        let display = format!("{}", plan);

        assert!(display.contains("Share Plan"));
        assert!(display.contains("share"));
        assert!(display.contains("80000"));
    }

    #[test]
    fn test_summary_counts() {
        let config = SharePlanConfig::deep().with_min_bytes(5000);

        let list = ValueInfo::new_container(
            ValueType::List,
            vec![
                ("big".to_string(), ValueInfo::new_atomic(ValueType::Double, 10000, 80000)),
                ("small".to_string(), ValueInfo::new_atomic(ValueType::Integer, 100, 400)),
                ("medium".to_string(), ValueInfo::new_atomic(ValueType::Double, 1000, 8000)),
            ],
        );

        let plan = share_plan(&list, &config);

        // List kept, big and medium shared, small kept
        assert_eq!(plan.summary().share_count, 2);
        assert_eq!(plan.summary().keep_count, 2); // list + small
        assert_eq!(plan.summary().total_share_bytes, 88000); // 80000 + 8000
    }

    #[test]
    fn test_bool_and_raw_types() {
        let config = SharePlanConfig::default().with_min_bytes(100);

        let bool_val = ValueInfo::new_atomic(ValueType::Logical, 10000, 10000);
        let raw_val = ValueInfo::new_atomic(ValueType::Raw, 10000, 10000);

        let bool_plan = share_plan(&bool_val, &config);
        let raw_plan = share_plan(&raw_val, &config);

        // Both should be shareable
        assert_eq!(bool_plan.nodes()[0].action, ShareAction::Share);
        assert_eq!(raw_plan.nodes()[0].action, ShareAction::Share);
        assert!(bool_plan.nodes()[0].shareable);
        assert!(raw_plan.nodes()[0].shareable);
    }
}
