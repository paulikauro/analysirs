use crate::circuit::{friendly_name, Circuit};
use crate::separate::CombinationalBlock;
use redpiler_graph::*;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, Default)]
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
    let mut delays: Vec<Delays> = vec![Default::default(); n];
    let mut visited = vec![0; n];
    let mut frontier = block.inputs.clone();
    while let Some(id) = frontier.pop() {
        let node = &circuit.nodes[id];
        delays[id] = node
            .inputs
            .iter()
            .map(|input| delays[input.to])
            .reduce(|a, b| a.combine(b))
            .unwrap_or_default()
            .add(node_delay(node));
        println!("at {} {:?} {:?}", id, node, delays[id]);
        for &output in &node.updates {
            visited[output] += 1;
            if visited[output] == circuit.nodes[output].inputs.len() {
                frontier.push(output);
            }
        }
    }
    delays[block.output]
}

fn topological_sort(circuit: &Circuit, block: &CombinationalBlock) -> Vec<NodeId> {
    let mut order = Vec::new();
    let mut stack = block.inputs.clone();
    let mut visited = HashSet::new();
    while let Some(id) = stack.pop() {
        if visited.contains(&id) {
            continue;
        }
        visited.insert(id);
        order.push(id);
        let node = &circuit.nodes[id];
        for neighbor in &node.updates {
            if !visited.contains(neighbor) {
                stack.push(*neighbor)
            }
        }
    }
    order.reverse();
    order
}

fn node_delay(node: &Node) -> usize {
    use NodeType::*;
    match node.ty {
        Repeater(delay) => delay as usize,
        Comparator(_) | Torch => 1,
        _ => 0,
    }
}
