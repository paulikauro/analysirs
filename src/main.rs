extern crate anyhow;
extern crate redpiler_graph;

mod bin_expr;
mod circuit;
mod separate;

use anyhow::Result;

fn main() -> Result<()> {
    let file = "tff";
    let circuit = circuit::load_circuit(&format!("examples/{}", file))?;
    let io = separate::find_io(&circuit);
    println!("io:\n{:?}", io);
    separate::print_io(&io, &circuit);
    let blocks = separate::find_combinational_blocks(&circuit, &io)?;
    println!("blocks:\n{:?}", blocks);
    separate::print_blocks(&blocks, &circuit);
    Ok(())
}
