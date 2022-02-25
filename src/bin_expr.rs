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

pub struct BoolPoly {
    nodes: Vec<PolyExpr>,
    memo: HashMap<PolyExpr, usize>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
enum PolyExpr {
    Zero,
    One,
    Var(usize),
    Add(usize, usize),
    Mul(usize, usize),
}

impl BoolPoly {
    pub fn new() -> BoolPoly {
        BoolPoly {
            nodes: Vec::new(),
            memo: HashMap::new(),
        }
    }
    pub fn print_node(&self, id: usize, var_map: &HashMap<usize, String>) {
        use PolyExpr::*;
        match &self.nodes[id] {
            Var(name) => print!("{}", var_map[name]),
            Zero => print!("0"),
            One => print!("1"),
            &Add(a, b) => {
                print!("(+ ");
                self.print_node(a, var_map);
                print!(" ");
                self.print_node(b, var_map);
                print!(")")
            }
            &Mul(a, b) => {
                print!("(* ");
                self.print_node(a, var_map);
                print!(" ");
                self.print_node(b, var_map);
                print!(")")
            }
        }
    }
    fn push(&mut self, node: PolyExpr) -> usize {
        *self.memo.entry(node.clone()).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len() - 1
        })
    }
    fn zero(&mut self) -> usize {
        self.push(PolyExpr::Zero)
    }
    fn one(&mut self) -> usize {
        self.push(PolyExpr::One)
    }
    fn sorted(&self, a: usize, b: usize) -> (&PolyExpr, usize, &PolyExpr, usize) {
        let ae = &self.nodes[a];
        let be = &self.nodes[b];
        if ae < be {
            (ae, a, be, b)
        } else {
            (be, b, ae, a)
        }
    }
    fn add(&mut self, x: usize, y: usize) -> usize {
        use PolyExpr::*;
        // sorting takes care of commutativity
        let (ae, a, be, b) = self.sorted(x, y);
        match (ae, be) {
            // associativity
            // (c + d) + b = c + (d + b)
            // (&Add(c, d), _) => {
            //     println!("add assoc {:?} {:?}", ae, be);
            //     let db = self.add(d, b);
            //     self.push(Add(c, db))
            //     // self.add(c, db)
            // }
            // identity
            // 0 + b = b
            (Zero, _) => b,
            // inverses
            // a + a = 0
            (ae, be) if ae == be => self.zero(),
            _ => self.push(Add(a, b)),
        }
    }
    fn mul(&mut self, x: usize, y: usize) -> usize {
        use PolyExpr::*;
        // sorting takes care of commutativity
        let (ae, a, be, b) = self.sorted(x, y);
        match (ae, be) {
            // associativity
            // (c * d) * b = c * (d * b)
            // (&Mul(c, d), _) => {
            //     let db = self.mul(d, b);
            //     self.push(Mul(c, db))
            //     // self.mul(c, db)
            // }
            // annihilation?
            (Zero, _) => self.zero(),
            // identity
            // 1 * b = b
            (One, _) => b,
            // boolean property?
            // a * a = a
            (ae, be) if ae == be => a,
            // distributivity
            // (c + d) * b = (c * b) + (d * b)
            // (&Add(c, d), _) => {
            //     let cb = self.mul(c, b);
            //     let db = self.mul(d, b);
            //     // self.push(Add(cb, db))
            //     self.add(cb, db)
            // }
            _ => self.push(Mul(a, b)),
        }
    }
}

impl BinExpr for BoolPoly {
    type Id = usize;
    type Name = usize;
    fn var(&mut self, name: Self::Name) -> Self::Id {
        self.push(PolyExpr::Var(name))
    }
    fn lit(&mut self, value: bool) -> Self::Id {
        match value {
            false => self.zero(),
            true => self.one(),
        }
    }
    fn not(&mut self, x: Self::Id) -> Self::Id {
        let one = self.one();
        self.add(x, one)
    }
    fn and(&mut self, x: Self::Id, y: Self::Id) -> Self::Id {
        self.mul(x, y)
    }
    fn or(&mut self, x: Self::Id, y: Self::Id) -> Self::Id {
        let x_plus_y = self.add(x, y);
        let x_times_y = self.mul(x, y);
        self.add(x_plus_y, x_times_y)
    }
}

pub struct TruthTable {
    // name to bit-id
    vars: HashMap<usize, usize>,
    // bit-id to name
    revs: HashMap<usize, usize>,
}

impl TruthTable {
    pub fn new() -> TruthTable {
        TruthTable {
            vars: HashMap::new(),
            revs: HashMap::new(),
        }
    }
    pub fn print_node(&self, id: u64, var_map: &HashMap<usize, String>) {
        let n = self.vars.len();
        for i in (0..n).rev() {
            let name = self.revs[&i];
            print!("{} ", var_map[&name]);
        }
        println!("| value");
        let rows = 1 << n;
        for row_val in 0..rows {
            let rvs = 1 << row_val;
            for i in (0..n).rev() {
                let x = bit_thing(i as u64) & rvs == 0;
                print!("{} ", x as u64)
            }
            let y = id & rvs == 0;
            println!("| {}", y as u64);
        }
    }
}

fn bit_thing(x: u64) -> u64 {
    match x {
        0 => 0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101,
        1 => 0b00110011_00110011_00110011_00110011_00110011_00110011_00110011_00110011,
        2 => 0b00001111_00001111_00001111_00001111_00001111_00001111_00001111_00001111,
        3 => 0b00000000_11111111_00000000_11111111_00000000_11111111_00000000_11111111,
        4 => 0b00000000_00000000_11111111_11111111_00000000_00000000_11111111_11111111,
        5 => 0b00000000_00000000_00000000_00000000_11111111_11111111_11111111_11111111,
        _ => panic!("too many variables"),
    }
}

impl BinExpr for TruthTable {
    type Id = u64;
    type Name = usize;
    fn var(&mut self, name: Self::Name) -> Self::Id {
        let len = self.vars.len();
        let bit_id = *self.vars.entry(name).or_insert(len);
        self.revs.insert(bit_id, name);
        bit_thing(bit_id as u64)
    }

    fn lit(&mut self, value: bool) -> Self::Id {
        match value {
            false => 0,
            true => std::u64::MAX,
        }
    }

    fn not(&mut self, x: Self::Id) -> Self::Id {
        !x
    }
    fn and(&mut self, x: Self::Id, y: Self::Id) -> Self::Id {
        x & y
    }
    fn or(&mut self, x: Self::Id, y: Self::Id) -> Self::Id {
        x | y
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum GraphNode {
    Var(usize),
    Lit(bool),
    Not(GraphId),
    And(GraphId, GraphId),
    Or(GraphId, GraphId),
}

pub struct GraphBinExpr {
    nodes: Vec<GraphNode>,
    memo: HashMap<GraphNode, usize>,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct GraphId(usize);

impl GraphBinExpr {
    fn push(&mut self, node: GraphNode) -> GraphId {
        GraphId(*self.memo.entry(node.clone()).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len() - 1
        }))
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

    pub fn new() -> GraphBinExpr {
        GraphBinExpr {
            nodes: Vec::new(),
            memo: HashMap::new(),
        }
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
        use GraphNode::*;
        match self.nodes[x.0] {
            Lit(value) => self.lit(!value),
            Not(y) => y,
            And(a, b) => {
                let na = self.not(a);
                let nb = self.not(b);
                self.or(na, nb)
            }
            Or(a, b) => {
                let na = self.not(a);
                let nb = self.not(b);
                self.and(na, nb)
            }
            _ => self.push(Not(x)),
        }
    }
    fn and(&mut self, x: GraphId, y: GraphId) -> GraphId {
        use GraphNode::*;
        match (&self.nodes[x.0], &self.nodes[y.0]) {
            (Lit(true), _) => y,
            (Lit(false), _) => self.lit(false),
            (_, Lit(true)) => x,
            (_, Lit(false)) => self.lit(false),
            (x, Not(y)) if *x == self.nodes[y.0] => self.lit(false),
            (Not(y), x) if *x == self.nodes[y.0] => self.lit(false),
            (a, b) if a == b => x,
            // distribute into OR
            (Or(a, b), _) => {
                let a = *a;
                let b = *b;
                let ay = self.and(a, y);
                let by = self.and(b, y);
                self.or(ay, by)
            }
            (_, Or(a, b)) => {
                let a = *a;
                let b = *b;
                let xa = self.and(x, a);
                let xb = self.and(x, b);
                self.or(xa, xb)
            }
            _ => self.push(And(x, y)),
        }
    }
    fn or(&mut self, x: GraphId, y: GraphId) -> GraphId {
        use GraphNode::*;
        match (&self.nodes[x.0], &self.nodes[y.0]) {
            (Lit(false), _) => y,
            (Lit(true), _) => self.lit(true),
            (_, Lit(false)) => x,
            (_, Lit(true)) => self.lit(true),
            (x, Not(y)) if *x == self.nodes[y.0] => self.lit(true),
            (Not(y), x) if *x == self.nodes[y.0] => self.lit(true),
            (a, b) if a == b => x,
            _ => self.push(Or(x, y)),
        }
    }
}
