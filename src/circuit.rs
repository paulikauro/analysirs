use anyhow::Result;
use redpiler_graph::{deserialize, BlockPos, Node, NodeId};
use serde::{Deserialize, Serialize};
use std::fs;

pub struct Circuit {
    pub nodes: Vec<Node>,
    /// Inverse of Node.inputs
    pub outputs: Vec<Vec<NodeId>>,
    pub meta: CircuitMeta,
}

#[derive(Serialize, Deserialize)]
pub struct CircuitMeta {
    pub names: Vec<Name>,
}

pub fn friendly_name(id: NodeId, circuit: &Circuit) -> String {
    // TODO: perhaps store names in a map instead
    let node = &circuit.nodes[id];
    circuit
        .meta
        .names
        .iter()
        .find(|x| x.pos == node.pos)
        .map(|x| x.name.clone())
        .unwrap_or_else(|| format!("{},{},{}", node.pos.x, node.pos.y, node.pos.z))
}

#[derive(Serialize, Deserialize)]
pub struct Name {
    pub name: String,
    pub pos: BlockPos,
}

pub fn load_circuit(basepath: &str) -> Result<Circuit> {
    let nodes = deserialize(&fs::read(format!("{}.bc", basepath))?)?;
    let meta = serde_json::from_slice(&fs::read(format!("{}.json", basepath))?)?;
    let outputs = construct_outputs(&nodes);
    Ok(Circuit {
        nodes,
        outputs,
        meta,
    })
}

fn construct_outputs(nodes: &[Node]) -> Vec<Vec<NodeId>> {
    let mut outputs = Vec::with_capacity(nodes.len());
    outputs.resize(nodes.len(), Vec::new());
    for (id, node) in nodes.iter().enumerate() {
        for link in &node.inputs {
            outputs[link.to].push(id);
        }
    }
    outputs
}
