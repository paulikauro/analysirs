use crate::bin_expr::BinExpr;
use crate::circuit::Circuit;
use crate::separate::{partition_inputs, CombinationalBlock};
use anyhow::bail;
use core::str::FromStr;
use egg::{rewrite as rw, *};
use redpiler_graph::{BlockPos, ComparatorMode, Link, LinkType, Node, NodeId, NodeType};
use std::collections::{HashMap, HashSet};
use std::fmt;
use Expr::*;

define_language! {
    pub enum Expr {
        "add" = Add([Id; 2]),
        "min" = Min([Id; 2]),
        "max" = Max([Id; 2]),
        "and" = And([Id; 2]),
        "or" = Or([Id; 2]),
        "neg" = Neg(Id),
        "sig" = Sig(Id),
        "not" = Not(Id),
        "bin" = Bin(Id),
        BConst(bool),
        SConst(i32),
        Input(NodeIdWrapper),
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
struct NodeIdWrapper(NodeId);

impl FromStr for NodeIdWrapper {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with("input") {
            bail!("not an inputs")
        }
        let ss = &s[5..];
        Ok(NodeIdWrapper(ss.parse::<usize>()?))
    }
}

impl fmt::Display for NodeIdWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "input{}", self.0)
    }
}

pub fn block_to_expr<B: BinExpr<Name = NodeId>>(
    b: &mut B,
    circuit: &Circuit,
    block: &CombinationalBlock,
) -> B::Id {
    let mut egraph: EGraph<Expr, ()> = Default::default();
    println!("constructing egraph");
    let id = add_to_egraph(
        &mut egraph,
        &circuit.nodes,
        &HashSet::from_iter(block.inputs.iter().cloned()),
        block.output,
    );
    let extractor = Extractor::new(&egraph, BooleanCostFunction);
    let (best_cost, best_expr) = extractor.find_best(id);
    println!("rewriting egraph {}", best_expr);
    let expr = rewrite(id, egraph);
    println!("booleanifying egraph {}", expr);
    do_booleanify(&expr, Id::from(expr.as_ref().len() - 1), b)
}

fn add_to_egraph(
    egraph: &mut EGraph<Expr, ()>,
    nodes: &[Node],
    block_inputs: &HashSet<NodeId>,
    id: NodeId,
) -> Id {
    if block_inputs.contains(&id) {
        let a = egraph.add(Input(NodeIdWrapper(id)));
        println!("input {} went to {}", id, a);
        return egraph.add(Sig(a));
    }
    fn make_input(
        egraph: &mut EGraph<Expr, ()>,
        nodes: &[Node],
        block_inputs: &HashSet<NodeId>,
        inputs: Vec<Link>,
    ) -> Id {
        let mut id = egraph.add(SConst(0));
        for input in inputs {
            let weight = egraph.add(SConst(-(input.weight as i32)));
            let real_input = add_to_egraph(egraph, nodes, block_inputs, input.to);
            let decayed = egraph.add(Add([real_input, weight]));
            id = egraph.add(Max([id, decayed]));
        }
        id
    }

    let node = &nodes[id];
    let (defaults, sides) = partition_inputs(node);
    let default_input = make_input(egraph, nodes, block_inputs, defaults);
    let side_input = make_input(egraph, nodes, block_inputs, sides);

    use NodeType::*;
    match node.ty {
        Lever => {
            // TODO: this will never happen?
            let a = egraph.add(Input(NodeIdWrapper(id)));
            egraph.add(Sig(a))
        }
        Constant => egraph.add(SConst(node.output_power as i32)),
        Repeater(..) => {
            let a = egraph.add(Bin(default_input));
            egraph.add(Sig(a))
        }
        Comparator(ComparatorMode::Subtract) => {
            let a = egraph.add(SConst(0));
            let b = egraph.add(Neg(side_input));
            let c = egraph.add(Add([default_input, b]));
            egraph.add(Max([a, c]))
        }
        Comparator(ComparatorMode::Compare) => {
            let a = egraph.add(SConst(0));
            let b = egraph.add(SConst(1));
            let c = egraph.add(Neg(side_input));
            let d = egraph.add(Add([default_input, c]));
            let e = egraph.add(Add([d, b]));
            let f = egraph.add(Bin(e));
            let g = egraph.add(Sig(f));
            let h = egraph.add(Add([default_input, g]));
            egraph.add(Max([a, h]))
        }
        Torch => {
            let a = egraph.add(Bin(default_input));
            let b = egraph.add(Not(a));
            egraph.add(Sig(b))
        }
        // this is Sig even though it might not want to be
        Lamp => {
            let a = egraph.add(Bin(default_input));
            egraph.add(Sig(a))
        }
        _ => panic!("cant do this one: {:?}", &node.ty),
    }
}

fn rewrite(id: Id, egraph: EGraph<Expr, ()>) -> RecExpr<Expr> {
    let mut rules: Vec<Rewrite<Expr, ()>> = vec![
        // commutativity
        rw!("comm-add"; "(add ?x ?y)" => "(add ?y ?x)"),
        rw!("comm-min"; "(min ?x ?y)" => "(min ?y ?x)"),
        rw!("comm-max"; "(max ?x ?y)" => "(max ?y ?x)"),
        rw!("comm-and"; "(and ?x ?y)" => "(and ?y ?x)"),
        rw!("comm-or"; "(or ?x ?y)" => "(or ?y ?x)"),
        // identity
        rw!("ident-add"; "(add ?x 0)" => "?x"),
        rw!("ident-and"; "(and ?x true)" => "?x"),
        rw!("ident-or"; "(or ?x false)" => "?x"),
        // idempotence
        rw!("idemp-min"; "(min ?x ?x)" => "?x"),
        rw!("idemp-max"; "(max ?x ?x)" => "?x"),
        rw!("idemp-and"; "(and ?x ?x)" => "?x"),
        rw!("idemp-or"; "(or ?x ?x)" => "?x"),
        // double negation
        rw!("neg-neg"; "(neg (neg ?x))" => "?x"),
        rw!("not-not"; "(not (not ?x))" => "?x"),
        // neg distributivity
        rw!("neg-add"; "(neg (add ?x ?y))" => "(add (neg ?x) (neg ?y))"),
        rw!("neg-min"; "(neg (min ?x ?y))" => "(max (neg ?x) (neg ?y))"),
        rw!("neg-max"; "(neg (max ?x ?y))" => "(min (neg ?x) (neg ?y))"),
        // add distributivity
        rw!("add-max"; "(add ?x (max ?y ?z))" => "(max (add ?x ?y) (add ?x ?z))"),
        rw!("add-min"; "(add ?x (min ?y ?z))" => "(min (add ?x ?y) (add ?x ?z))"),
        // bin distributivity
        rw!("bin-min"; "(bin (min ?x ?y))" => "(and (bin ?x) (bin ?y))"),
        rw!("bin-max"; "(bin (max ?x ?y))" => "(or (bin ?x) (bin ?y))"),
        // other
        rw!("bin-sig"; "(bin (sig ?x))" => "?x"),
    ];
    rules.extend(
        vec![
            // associativity
            rw!("assoc-add"; "(add (add ?x ?y) ?z)" <=> "(add ?x (add ?y ?z))"),
            rw!("assoc-min"; "(min (min ?x ?y) ?z)" <=> "(min ?x (min ?y ?z))"),
            rw!("assoc-max"; "(max (max ?x ?y) ?z)" <=> "(max ?x (max ?y ?z))"),
            rw!("assoc-and"; "(and (and ?x ?y) ?z)" <=> "(and ?x (and ?y ?z))"),
            rw!("assoc-or"; "(or (or ?x ?y) ?z)" <=> "(or ?x (or ?y ?z))"),
        ]
        .concat(),
    );
    let runner = Runner::default().with_egraph(egraph).run(&rules[..]);
    let extractor = Extractor::new(&runner.egraph, BooleanCostFunction);
    let (best_cost, best_expr) = extractor.find_best(id);
    best_expr
}

fn invert_varmap(vars: HashMap<NodeId, usize>) -> HashMap<usize, NodeId> {
    vars.iter().map(|(x, y)| (*y, *x)).collect()
}

fn do_booleanify<B: BinExpr<Name = NodeId>>(e: &RecExpr<Expr>, id: Id, b: &mut B) -> B::Id {
    println!("do_booleanify {} {:?}", id, e[id]);
    match &e[id] {
        Sig(x) => do_booleanify(e, *x, b),
        Not(x) => {
            let x = do_booleanify(e, *x, b);
            b.not(x)
        }
        And([x, y]) => {
            let x = do_booleanify(e, *x, b);
            let y = do_booleanify(e, *y, b);
            b.and(x, y)
        }
        Or([x, y]) => {
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
                    if let SConst(value) = node {
                        b.lit(*value > 0)
                    } else {
                        panic!("do_booleanify: (bin (neg not-const))")
                    }
                }
                Add([x, y]) => {
                    let (mut constant, mut addends) = flatten_add(e, *x);
                    let (other_constant, other_addends) = flatten_add(e, *y);
                    addends.extend(other_addends);
                    constant += other_constant;
                    // local_inputs is effectively a set of the inputs to this node
                    let mut local_inputs: Vec<usize> = addends.iter().map(|x| x.0).collect();
                    local_inputs.sort_unstable();
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
                                for &input in &local_inputs {
                                    let is_set = var_from_subst(input, subst);
                                    let mut literal = b.var(input);
                                    if is_set {
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
        Input(name) => b.var(name.0),
        _ => panic!("do_booleanify: cannot handle this"),
    }
}

fn var_from_subst(index: usize, subst: u64) -> bool {
    subst & (1 << index) == 0
}

fn eval_addends(
    addends: &[Addend],
    constant: i32,
    var_map: &HashMap<NodeId, usize>,
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

struct Addend(NodeId, bool);

/// Flatten a tree of addition nodes into a single constant and a vec of other inputs.
fn flatten_add(e: &RecExpr<Expr>, start: Id) -> (i32, Vec<Addend>) {
    let mut addends = Vec::new();
    let mut constant = 0i32;
    let mut frontier = vec![start];
    while let Some(id) = frontier.pop() {
        match &e[id] {
            Add(x) => frontier.extend(x),
            SConst(value) => constant += value,
            Neg(x) => match &e[*x] {
                SConst(value) => constant -= value,
                Sig(y) => {
                    if let Input(name) = &e[*y] {
                        addends.push(Addend(name.0, true))
                    } else {
                        // TODO: wut
                        panic!("flatten_add: (neg (sig not-input))")
                    }
                }
                _ => panic!("flatten_add: (neg not-const-or-sig)"),
            },
            Sig(x) => {
                if let Input(name) = &e[*x] {
                    println!("WELL i got an input here!!! {}", name);
                    addends.push(Addend(name.0, false))
                } else {
                    // TODO: wut
                    panic!("flatten_add: (sig not-input) {:?} {:?}", x, e[*x])
                }
            }
            _ => panic!("flatten_add: not supported"),
        }
    }
    (constant, addends)
}

struct BooleanCostFunction;
impl CostFunction<Expr> for BooleanCostFunction {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &Expr, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            Min(..) | Max(..) => 1,
            _ => 0,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
