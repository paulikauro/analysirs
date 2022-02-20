use crate::bin_expr::BinExpr;
use crate::circuit::Circuit;
use crate::separate::{partition_inputs, CombinationalBlock};
use anyhow::bail;
use redpiler_graph::{ComparatorMode, Link, Node, NodeId, NodeType};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ops::{Index, IndexMut};
use Expr::*;

type Id = usize;
#[derive(Debug)]
pub enum Expr {
    Add(Id, Id),
    Min(Id, Id),
    Max(Id, Id),
    And(Id, Id),
    Or(Id, Id),
    Neg(Id),
    Sig(Id),
    Not(Id),
    Bin(Id),
    BConst(bool),
    SConst(i32),
    Input(NodeId),
}

struct Graph {
    nodes: Vec<Expr>,
}

impl Index<Id> for Graph {
    type Output = Expr;
    fn index(&self, id: Id) -> &Expr {
        &self.nodes[id]
    }
}

impl IndexMut<Id> for Graph {
    fn index_mut(&mut self, id: Id) -> &mut Expr {
        &mut self.nodes[id]
    }
}

impl Graph {
    fn push(&mut self, e: Expr) -> Id {
        self.nodes.push(e);
        self.nodes.len() - 1
    }

    fn add(&mut self, e: Expr) -> Id {
        match &e {
            // add distributivity
            Add(x, a) => match self.nodes[*a] {
                Max(y, z) => {
                    let xy = self.add(Add(*x, y));
                    let xz = self.add(Add(*x, z));
                    self.add(Max(xy, xz))
                }
                Min(y, z) => {
                    let xy = self.add(Add(*x, y));
                    let xz = self.add(Add(*x, z));
                    self.push(Min(xy, xz))
                }
                _ => self.push(e),
            },
            // neg distributivity and double negation
            Neg(a) => match self.nodes[*a] {
                Add(x, y) => {
                    let nx = self.add(Neg(x));
                    let ny = self.add(Neg(y));
                    self.push(Add(nx, ny))
                }
                Min(x, y) => {
                    let nx = self.add(Neg(x));
                    let ny = self.add(Neg(y));
                    self.push(Max(nx, ny))
                }
                Max(x, y) => {
                    let nx = self.add(Neg(x));
                    let ny = self.add(Neg(y));
                    self.push(Min(nx, ny))
                }
                Neg(x) => x,
                _ => self.push(e),
            },
            // bin distributivity and bin-sig
            Bin(a) => match self.nodes[*a] {
                Min(x, y) => {
                    let bx = self.add(Bin(x));
                    let by = self.add(Bin(y));
                    self.push(And(bx, by))
                }
                Max(x, y) => {
                    let bx = self.add(Bin(x));
                    let by = self.add(Bin(y));
                    self.push(Or(bx, by))
                }
                Sig(x) => x,
                _ => self.push(e),
            },
            // double negation (boolean)
            Not(a) => match self.nodes[*a] {
                Not(x) => x,
                _ => self.push(e),
            },
            _ => self.push(e),
            // Min(Id, Id),
            // Max(Id, Id),
            // And(Id, Id),
            // Or(Id, Id),
            // Sig(Id),
            // Not(Id),
            // Bin(Id),
            // BConst(bool),
            // SConst(i32),
            // Input(NodeId),
        }
    }
    pub fn print_node(&self, id: Id, vars: &HashMap<usize, String>) {
        match &self.nodes[id] {
            Add(a, b) => {
                print!("(add ");
                self.print_node(*a, vars);
                print!(" ");
                self.print_node(*b, vars);
                print!(")");
            }
            Min(a, b) => {
                print!("(min ");
                self.print_node(*a, vars);
                print!(" ");
                self.print_node(*b, vars);
                print!(")");
            }
            Max(a, b) => {
                print!("(max ");
                self.print_node(*a, vars);
                print!(" ");
                self.print_node(*b, vars);
                print!(")");
            }
            And(a, b) => {
                print!("(and ");
                self.print_node(*a, vars);
                print!(" ");
                self.print_node(*b, vars);
                print!(")");
            }
            Or(a, b) => {
                print!("(or ");
                self.print_node(*a, vars);
                print!(" ");
                self.print_node(*b, vars);
                print!(")");
            }
            Neg(a) => {
                print!("(neg ");
                self.print_node(*a, vars);
                print!(")");
            }
            Sig(a) => {
                print!("(sig ");
                self.print_node(*a, vars);
                print!(")");
            }
            Not(a) => {
                print!("(not ");
                self.print_node(*a, vars);
                print!(")");
            }
            Bin(a) => {
                print!("(bin ");
                self.print_node(*a, vars);
                print!(")");
            }
            BConst(value) => print!("{}", value),
            SConst(value) => print!("{}", value),
            Input(input) => print!("(input {})", input),
        }
    }
}

pub fn block_to_expr<
    B: BinExpr<Name = NodeId, Id: Ord + PartialEq + Debug + Hash + Clone + Copy>,
>(
    b: &mut B,
    circuit: &Circuit,
    block: &CombinationalBlock,
    input_names: &HashMap<usize, String>,
) -> B::Id {
    let mut graph = Graph { nodes: Vec::new() };
    println!("constructing graph");
    let id = add_to_graph(
        &mut graph,
        &circuit.nodes,
        &HashSet::from_iter(block.inputs.iter().cloned()),
        block.output,
    );
    graph.print_node(id, input_names);
    println!();
    println!("booleanifying graph");
    do_booleanify(&graph, id, b)
}

fn add_to_graph(
    graph: &mut Graph,
    nodes: &[Node],
    block_inputs: &HashSet<NodeId>,
    id: NodeId,
) -> Id {
    if block_inputs.contains(&id) {
        let a = graph.add(Input(id));
        return graph.add(Sig(a));
    }
    fn make_input(
        graph: &mut Graph,
        nodes: &[Node],
        block_inputs: &HashSet<NodeId>,
        inputs: Vec<Link>,
    ) -> Id {
        let mut id = graph.add(SConst(0));
        for input in inputs {
            let weight = graph.add(SConst(-(input.weight as i32)));
            let real_input = add_to_graph(graph, nodes, block_inputs, input.to);
            let decayed = graph.add(Add(real_input, weight));
            id = graph.add(Max(id, decayed));
        }
        id
    }

    let node = &nodes[id];
    let (defaults, sides) = partition_inputs(node);
    let default_input = make_input(graph, nodes, block_inputs, defaults);
    let side_input = make_input(graph, nodes, block_inputs, sides);

    use NodeType::*;
    match node.ty {
        Lever => {
            // TODO: this will never happen?
            let a = graph.add(Input(id));
            graph.add(Sig(a))
        }
        Constant => graph.add(SConst(node.output_power as i32)),
        Repeater(..) => {
            let a = graph.add(Bin(default_input));
            graph.add(Sig(a))
        }
        Comparator(ComparatorMode::Subtract) => {
            let a = graph.add(SConst(0));
            let b = graph.add(Neg(side_input));
            let c = graph.add(Add(default_input, b));
            graph.add(Max(a, c))
        }
        Comparator(ComparatorMode::Compare) => {
            let a = graph.add(SConst(0));
            let b = graph.add(SConst(1));
            let c = graph.add(Neg(side_input));
            let d = graph.add(Add(default_input, c));
            let e = graph.add(Add(d, b));
            let f = graph.add(Bin(e));
            let g = graph.add(Sig(f));
            let h = graph.add(Add(default_input, g));
            graph.add(Max(a, h))
        }
        Torch => {
            let a = graph.add(Bin(default_input));
            let b = graph.add(Not(a));
            graph.add(Sig(b))
        }
        // this is Sig even though it might not want to be
        Lamp => {
            let a = graph.add(Bin(default_input));
            graph.add(Sig(a))
        }
        _ => panic!("cant do this one: {:?}", &node.ty),
    }
}

fn invert_varmap(vars: HashMap<NodeId, usize>) -> HashMap<usize, NodeId> {
    vars.iter().map(|(x, y)| (*y, *x)).collect()
}

fn do_booleanify<B: BinExpr<Name = NodeId, Id: Ord + PartialEq + Debug + Hash + Clone + Copy>>(
    e: &Graph,
    id: Id,
    b: &mut B,
) -> B::Id {
    println!("do_booleanify {} {:?}", id, e[id]);
    match &e[id] {
        Sig(x) => do_booleanify(e, *x, b),
        Not(x) => {
            let x = do_booleanify(e, *x, b);
            b.not(x)
        }
        And(x, y) => {
            let x = do_booleanify(e, *x, b);
            let y = do_booleanify(e, *y, b);
            b.and(x, y)
        }
        Or(x, y) => {
            let x = do_booleanify(e, *x, b);
            let y = do_booleanify(e, *y, b);
            b.or(x, y)
        }
        Bin(x) => {
            println!("BIN {}, {:?}", x, &e[*x]);
            match &e[*x] {
                SConst(value) => b.lit(*value > 0),
                Neg(y) => {
                    let node = &e[*y];
                    match node {
                        SConst(value) => b.lit(*value > 0),
                        // sig of anything >= 0, neg of that < 0, so not > 0 which is false
                        Sig(_) => b.lit(false),
                        _ => panic!("do_booleanify: (bin (neg not-const)), {} {:?}", y, e[*y]),
                    }
                }
                Add(x, y) => {
                    let (mut constant, mut addends) = flatten_add(e, *x, b);
                    let (other_constant, other_addends) = flatten_add(e, *y, b);
                    addends.extend(other_addends);
                    constant += other_constant;
                    // local_inputs is effectively a set of the inputs to this node
                    let mut local_inputs: Vec<B::Id> = addends.iter().map(|x| x.0).collect();
                    local_inputs.sort();
                    local_inputs.dedup();
                    println!(
                        "generating add, local inputs: {:?} constant: {}",
                        &local_inputs, constant
                    );
                    let var_map = local_inputs
                        .iter()
                        .enumerate()
                        .map(|(i, name)| (*name, i))
                        .collect();
                    let n = local_inputs.len();
                    if n == 0 {
                        b.lit(eval_addends(&addends[..], constant, &var_map, 0) > 0)
                    } else {
                        let mut sum_of_products = b.lit(false);
                        for subst in 0..2u64.pow(n as u32) {
                            let output = eval_addends(&addends[..], constant, &var_map, subst);
                            println!("subst {} output {}", subst, output);
                            if output > 0 {
                                let mut minterm = b.lit(true);
                                for input in &local_inputs {
                                    let is_set = var_from_subst(var_map[input], subst);
                                    let mut literal = *input;
                                    if !is_set {
                                        literal = b.not(literal);
                                    }
                                    minterm = b.and(minterm, literal);
                                }
                                sum_of_products = b.or(sum_of_products, minterm);
                            }
                        }
                        sum_of_products
                    }
                }
                _ => panic!("do_booleanify: cannot handle (bin this)"),
            }
        }
        Input(name) => b.var(*name),
        _ => panic!("do_booleanify: cannot handle this"),
    }
}

fn var_from_subst(index: usize, subst: u64) -> bool {
    subst & (1 << index) == 0
}

fn eval_addends<I: Hash + Eq>(
    addends: &[Addend<I>],
    constant: i32,
    var_map: &HashMap<I, usize>,
    subst: u64,
) -> i32 {
    fn inv_to_neg(x: bool) -> i32 {
        match x {
            true => -1,
            false => 1,
        }
    }
    let added: i32 = addends
        .iter()
        .map(|Addend(name, inv)| {
            inv_to_neg(*inv) * var_from_subst(var_map[name], subst) as i32 * 15
        })
        .sum();
    added + constant
}

struct Addend<I>(I, bool);

/// Flatten a tree of addition nodes into a single constant and a vec of other inputs.
fn flatten_add<B: BinExpr<Name = NodeId, Id: Ord + PartialEq + Debug + Hash + Clone + Copy>>(
    e: &Graph,
    start: Id,
    b: &mut B,
) -> (i32, Vec<Addend<B::Id>>) {
    let mut addends = Vec::new();
    let mut constant = 0i32;
    let mut frontier = vec![start];
    while let Some(id) = frontier.pop() {
        match &e[id] {
            Add(x, y) => frontier.extend([x, y]),
            SConst(value) => constant += value,
            Neg(x) => match &e[*x] {
                SConst(value) => constant -= value,
                Sig(y) => {
                    let y_b = do_booleanify(e, *y, b);
                    addends.push(Addend(y_b, true))
                }
                _ => panic!("flatten_add: (neg not-const-or-sig) {} {:?}", x, e[*x]),
            },
            Sig(x) => {
                let x_b = do_booleanify(e, *x, b);
                addends.push(Addend(x_b, false))
            }
            _ => panic!("flatten_add: not supported {} {:?}", id, e[id]),
        }
    }
    (constant, addends)
}
