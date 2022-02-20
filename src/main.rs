#![feature(associated_type_bounds)]

mod bin_expr;
mod bool;
mod circuit;
mod separate;

use anyhow::Result;
use circuit::friendly_name;
use std::collections::HashMap;

fn main() -> Result<()> {
    let file = "not2";
    let circuit = circuit::load_circuit(&format!("examples/{}", file))?;
    let io = separate::find_io(&circuit);
    println!("io:\n{:?}", io);
    separate::print_io(&io, &circuit);
    let blocks = separate::find_combinational_blocks(&circuit, &io)?;
    println!("blocks:\n{:?}", blocks);
    separate::print_blocks(&blocks, &circuit);
    for block in blocks {
        println!("processing block {:?}", block);
        let mut b = bin_expr::GraphBinExpr::new();
        let input_names: HashMap<usize, String> = block
            .inputs
            .iter()
            .map(|&input| (input, friendly_name(input, &circuit)))
            .collect();
        let id = bool::block_to_expr(&mut b, &circuit, &block, &input_names);
        print!("{} = ", friendly_name(block.output, &circuit));
        b.print_node(id, &input_names);
        println!();
    }
    Ok(())
}
