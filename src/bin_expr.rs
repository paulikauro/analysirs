use std::collections::HashMap;

pub trait BinExpr {
    type Id;
    type Name;
    fn var(&mut self, name: Self::Name) -> Self::Id;
    fn lit(&mut self, value: bool) -> Self::Id;
    fn not(&mut self, x: Self::Id) -> Self::Id;
    fn and(&mut self, x: Self::Id, y: Self::Id) -> Self::Id;
    fn or(&mut self, x: Self::Id, y: Self::Id) -> Self::Id;
}

enum GraphNode {
    Var(usize),
    Lit(bool),
    Not(GraphId),
    And(GraphId, GraphId),
    Or(GraphId, GraphId),
}

pub struct GraphBinExpr {
    nodes: Vec<GraphNode>,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct GraphId(usize);

impl GraphBinExpr {
    fn push(&mut self, node: GraphNode) -> GraphId {
        self.nodes.push(node);
        GraphId(self.nodes.len() - 1)
    }
    pub fn print_node(&self, id: GraphId, vars: &HashMap<usize, String>) {
        use GraphNode::*;
        match &self.nodes[id.0] {
            Var(name) => print!("{}", vars[name]),
            Lit(value) => print!("{}", value),
            Not(x) => {
                print!("(not ");
                self.print_node(*x, vars);
                print!(")")
            }
            And(x, y) => {
                print!("(and ");
                self.print_node(*x, vars);
                print!(" ");
                self.print_node(*y, vars);
                print!(")")
            }
            Or(x, y) => {
                print!("(or ");
                self.print_node(*x, vars);
                print!(" ");
                self.print_node(*y, vars);
                print!(")")
            }
        }
    }
}

impl GraphBinExpr {
    fn new() -> GraphBinExpr {
        GraphBinExpr { nodes: Vec::new() }
    }
}

impl BinExpr for GraphBinExpr {
    type Id = GraphId;
    type Name = usize;

    fn var(&mut self, name: usize) -> GraphId {
        self.push(GraphNode::Var(name))
    }
    fn lit(&mut self, value: bool) -> GraphId {
        self.push(GraphNode::Lit(value))
    }
    fn not(&mut self, x: GraphId) -> GraphId {
        self.push(GraphNode::Not(x))
    }
    fn and(&mut self, x: GraphId, y: GraphId) -> GraphId {
        self.push(GraphNode::And(x, y))
    }
    fn or(&mut self, x: GraphId, y: GraphId) -> GraphId {
        self.push(GraphNode::Or(x, y))
    }
}
