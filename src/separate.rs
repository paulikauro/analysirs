use crate::circuit::{friendly_name, Circuit};
use anyhow::{bail, Result};
use redpiler_graph::*;
use std::collections::HashSet;

#[derive(Debug)]
pub struct IOInfo {
    pub inputs: HashSet<NodeId>,
    pub outputs: HashSet<NodeId>,
    pub latches: HashSet<NodeId>,
}

/// Find inputs, outputs and latches (only repeater locks supported) in the circuit.
pub fn find_io(circuit: &Circuit) -> IOInfo {
    let mut info = IOInfo {
        inputs: HashSet::new(),
        outputs: HashSet::new(),
        latches: HashSet::new(),
    };
    for (id, node) in circuit.nodes.iter().enumerate() {
        match node.ty {
            NodeType::Lever => {
                info.inputs.insert(id);
            }
            NodeType::Lamp => {
                info.outputs.insert(id);
            }
            NodeType::Repeater(..) => {
                let (default, side) = partition_inputs(node);
                let data_inputs: Vec<NodeId> = default.into_iter().map(|x| x.to).collect();
                let clock_inputs: Vec<NodeId> = side.into_iter().map(|x| x.to).collect();
                if clock_inputs.is_empty() {
                    // not being locked
                    continue;
                }
                info.latches.insert(id);
                info.inputs.insert(id);
                // TODO: ???
                info.outputs.extend(data_inputs);
                // TODO: are clock inputs needed? map instead of set
            }
            _ => {}
        }
    }
    info
}

pub fn print_io(io: &IOInfo, circuit: &Circuit) {
    println!("Inputs:");
    io.inputs
        .iter()
        .for_each(|&x| println!("\t{}", friendly_name(x, circuit)));
    println!("Outputs:");
    io.outputs
        .iter()
        .for_each(|&x| println!("\t{}", friendly_name(x, circuit)));
    println!("Latches:");
    io.latches
        .iter()
        .for_each(|&x| println!("\t{}", friendly_name(x, circuit)));
}

#[derive(Debug)]
pub struct CombinationalBlock {
    pub inputs: Vec<NodeId>,
    pub output: NodeId,
}

/// Find combinational logic blocks.
/// This also finds illegal cycles, ie. cycles that aren't broken by a latch.
/// Those would cause recursion in the combinational logic, which is hard to analyze
/// and may not even have a stable state in general.
pub fn find_combinational_blocks(
    circuit: &Circuit,
    io: &IOInfo,
) -> Result<Vec<CombinationalBlock>> {
    // inefficient implementation, search for inputs reachable from each output
    let mut blocks = Vec::new();
    let n = circuit.nodes.len();
    let mut states = Vec::with_capacity(n);
    for &output in &io.outputs {
        let mut inputs = Vec::new();
        states.clear();
        states.resize(n, NodeState::Unvisited);
        find_inputs(circuit, io, &mut inputs, &mut states, output)?;
        blocks.push(CombinationalBlock { output, inputs });
    }
    Ok(blocks)
}

pub fn print_blocks(blocks: &[CombinationalBlock], circuit: &Circuit) {
    for (i, block) in blocks.iter().enumerate() {
        println!(
            "Block {}, output {}: ",
            i,
            friendly_name(block.output, circuit)
        );
        for &input in block.inputs.iter() {
            println!("\tInput: {}", friendly_name(input, circuit));
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeState {
    Unvisited,
    Visiting,
    Visited,
}

fn find_inputs(
    circuit: &Circuit,
    io: &IOInfo,
    inputs: &mut Vec<NodeId>,
    states: &mut Vec<NodeState>,
    id: NodeId,
) -> Result<()> {
    use NodeState::*;
    match states[id] {
        Visiting => bail!("Found an illegal cycle"),
        Visited => return Ok(()),
        Unvisited => {}
    }
    if io.inputs.contains(&id) {
        inputs.push(id);
        states[id] = Visited;
        return Ok(());
    }
    states[id] = Visiting;
    for neighbor in &circuit.nodes[id].inputs {
        find_inputs(circuit, io, inputs, states, neighbor.to)?;
    }
    states[id] = Visited;
    Ok(())
}

/// Partition inputs into (default inputs, side inputs).
pub fn partition_inputs(node: &Node) -> (Vec<Link>, Vec<Link>) {
    node.inputs
        .iter()
        .cloned()
        .partition(|x| x.ty == LinkType::Default)
}
