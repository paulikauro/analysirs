use crate::circuit::{friendly_name, Circuit};
use crate::separate::CombinationalBlock;
use redpiler_graph::*;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
pub struct Delays {
    pub contamination: usize,
    pub propagation: usize,
}

impl Delays {
    fn combine(self, other: Delays) -> Delays {
        Delays {
            contamination: self.contamination.min(other.contamination),
            propagation: self.propagation.max(other.propagation),
        }
    }
    fn add(self, value: usize) -> Delays {
        Delays {
            contamination: self.contamination + value,
            propagation: self.propagation + value,
        }
    }
}

pub fn calculate_delays(circuit: &Circuit, block: &CombinationalBlock) -> Delays {
    let n = circuit.nodes.len();
    let mut delays: Vec<Delays> = vec![
        Delays {
            contamination: std::usize::MAX,
            propagation: std::usize::MIN
        };
        n
    ];
    for id in topological_sort(circuit, block) {
        let node = &circuit.nodes[id];
        delays[id] = node
            .inputs
            .iter()
            .map(|input| delays[input.to])
            .reduce(|a, b| a.combine(b))
            .unwrap_or(Delays {
                contamination: 0,
                propagation: 0,
            })
            .add(node_delay(node));
    }
    delays[block.output]
}

fn topological_sort(circuit: &Circuit, block: &CombinationalBlock) -> Vec<NodeId> {
    let mut order = Vec::new();
    let mut visited = HashSet::new();
    for &input in &block.inputs {
        topological_sort_dfs(circuit, block, input, &mut order, &mut visited);
    }
    order.reverse();
    order
}

fn topological_sort_dfs(
    circuit: &Circuit,
    block: &CombinationalBlock,
    id: NodeId,
    order: &mut Vec<NodeId>,
    visited: &mut HashSet<NodeId>,
) {
    if visited.contains(&id) {
        return;
    }
    visited.insert(id);
    for &neighbor in &circuit.outputs[id] {
        topological_sort_dfs(circuit, block, neighbor, order, visited);
    }
    order.push(id);
}

fn node_delay(node: &Node) -> usize {
    use NodeType::*;
    match node.ty {
        Repeater(delay) => delay as usize,
        Comparator(_) | Torch => 1,
        _ => 0,
    }
}
