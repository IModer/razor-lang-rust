type ℕ = u16;

use pest::{Parser, error::Error};
use std::io::{self, BufRead};

#[derive(pest_derive::Parser)]
#[grammar = "./razor/razor.pest"]
pub struct RazorParser;

#[derive(PartialEq)]
pub enum RazorASTTerm {
    True,
    False,
    IfThenElse {
        cond : Box<RazorASTTerm>,
        lb : Box<RazorASTTerm>,
        rb : Box<RazorASTTerm>
    },
    Num(ℕ),
    IsZero(Box<RazorASTTerm>),
    Plus {
        lhs : Box<RazorASTTerm>,
        rhs : Box<RazorASTTerm>
    },
}

impl std::fmt::Display for RazorASTTerm  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.serialize_term())
    }
}

impl RazorASTTerm {
    
    fn serialize_term(&self) -> String
    {
        use RazorASTTerm::*;

        match self {
            True                          => "true".to_string(),
            False                         => "false".to_string(),
            IfThenElse { cond, lb, rb }   => format!("(if {} then {} else {})", cond.serialize_term(), lb.serialize_term(), rb.serialize_term()),
            Num(n)                        => format!("num {}", n.to_string()),
            IsZero( u )                   => format!("(isZero {})", u.serialize_term()),
            Plus { lhs, rhs }             => format!("({} + {})", lhs.serialize_term(), rhs.serialize_term()),
        }
    }
    
    pub fn parse_term(s : String) -> Result<RazorASTTerm, Error<Rule>>
    {
        let program = RazorParser::parse(Rule::program, &s)?.next().unwrap();
        
        use pest::iterators::Pair;
        use RazorASTTerm::*;

        fn parse_value(pair: Pair<Rule>) -> RazorASTTerm {

            match pair.as_rule() {
                Rule::IfThenElse    =>  {let mut a = pair.into_inner();
                                        IfThenElse {
                                            cond : Box::new(parse_value(a.next().unwrap())),
                                            lb : Box::new(parse_value(a.next().unwrap())),
                                            rb : Box::new(parse_value(a.next().unwrap()))
                                        }},
                Rule::Plus          =>  {let mut a = pair.into_inner();
                                        Plus {
                                            lhs : Box::new(parse_value(a.next().unwrap())),
                                            rhs : Box::new(parse_value(a.next().unwrap()))
                                        }},
                Rule::isZero        => IsZero(Box::new(parse_value(pair.into_inner().next().unwrap()))),
                Rule::num           => Num(pair.into_inner().next().unwrap().as_str().parse().unwrap()),
                Rule::r#true        => True,
                Rule::r#false       => False,
                _    => unreachable!("Rule : {:?} at Pos : {:?}", pair.as_rule(), pair.as_span()),
            }
        }
    
        Ok(parse_value(program))
    }
    
    pub fn eval_term(t : RazorASTTerm) -> RazorASTTerm
    {
        use RazorASTTerm::*;
    
        match t {
            IfThenElse { cond, lb, rb } =>  if Self::eval_term(*cond) == True { Self::eval_term(*lb) } else { Self::eval_term(*rb) },
            IsZero(u)                   =>  {   
                                                match Self::eval_term(*u) {
                                                    Num(n) => if n == 0 {True} else {False},
                                                    _ => unreachable!("IsZero called with non number term")
                                            }},
            Plus { lhs, rhs }           => {
                                                let mut sum = 0;
                                                match Self::eval_term(*lhs) {
                                                    Num(n) => sum += n,
                                                    _ => unreachable!("Plus called with non number term on left hand side")
                                                }
                                                match Self::eval_term(*rhs) {
                                                    Num(n) => sum += n,
                                                    _ => unreachable!("Plus called with non number term on right hand side")
                                                }
                                                Num(sum)
                                            },
            Num(n)                      => Num(n),
            True                        => True,
            False                       => False,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct RazorWTTerm {
    variant : RazorWTVariant,
    typ : RazorWTType
}

// RazorType
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum RazorWTType {
    Nat, Bool
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum RazorWTVariant {
    True,
    False,
    IfThenElse {
        cond : Box<RazorWTTerm>,
        lb : Box<RazorWTTerm>,
        rb : Box<RazorWTTerm>
    },
    Num(ℕ),
    IsZero(Box<RazorWTTerm>),
    Plus {
        lhs : Box<RazorWTTerm>,
        rhs : Box<RazorWTTerm>
    },
}

impl std::fmt::Display for RazorWTTerm  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.serialize_term())
    }
}

impl RazorWTTerm {

    // TODO: RazorWTTerm::new()
    // think about the `very janky` part
    // think about not cloneing, copying?

    fn serialize_term(&self) -> String
    {
        use RazorWTVariant::*;

        match &self.variant {
            True                          => "[Bool](true)".to_string(),
            False                         => "[Bool](false)".to_string(),
            IfThenElse { cond, lb, rb }   => format!("[{:?}](if {} then {} else {})", &self.typ, cond.serialize_term(), lb.serialize_term(), rb.serialize_term()),
            Num(n)                        => format!("[{:?}](num {})", &self.typ, n.to_string()),
            IsZero( u )                   => format!("[{:?}](isZero {})",&self.typ, u.serialize_term()),
            Plus { lhs, rhs }             => format!("[{:?}]({} + {})",&self.typ, lhs.serialize_term(), rhs.serialize_term()),
        }
    }

    pub fn parse_term(s : String) -> Result<RazorWTTerm, Error<Rule>>
    {
        let program = RazorParser::parse(Rule::program, &s)?.next().unwrap();
        
        use pest::iterators::Pair;
        use RazorWTType::*;
        use RazorWTVariant::*;
    
        fn parse_value(pair: Pair<Rule>) -> RazorWTTerm {
            match pair.as_rule() {
                Rule::IfThenElse    =>  {
                                            let mut inner_pairs = pair.into_inner();
                                            let cond = parse_value(inner_pairs.next().unwrap());
                                            let lb = parse_value(inner_pairs.next().unwrap());
                                            let rb = parse_value(inner_pairs.next().unwrap());
                                            assert!(cond.typ == Bool, "term for the condition of IfThenElse should have type Bool");
                                            assert!(lb.typ == rb.typ, "terms on the two branches of IfThenElse should have the same type");
                                            let lb_typ = lb.typ.clone();  //This is very janky
                                            RazorWTTerm { variant : IfThenElse { cond: Box::new(cond), lb: Box::new(lb), rb: Box::new(rb) } , typ : lb_typ }
                                        },
                Rule::Plus          =>  {
                                            let mut inner_pairs = pair.into_inner();
                                            let lhs = parse_value(inner_pairs.next().unwrap());
                                            let rhs = parse_value(inner_pairs.next().unwrap());
                                            assert!(lhs.typ == Nat && rhs.typ == Nat, "terms on the two sides of Plus should have type Nat");
                                            RazorWTTerm { variant : Plus { lhs : Box::new(lhs), rhs : Box::new(rhs) }, typ : Nat }
                                        },
                Rule::isZero        =>  {
                                            let inner = parse_value(pair.into_inner().next().unwrap());
                                            assert!(inner.typ == Nat, "terms inside IsZero should have type Nat");  // Basically type checking 
                                            RazorWTTerm { variant : IsZero(Box::new(inner)), typ : Bool }
                                        }
                Rule::num           => RazorWTTerm { variant : Num(pair.into_inner().next().unwrap().as_str().parse().unwrap()), typ : Nat},
                Rule::r#true        => RazorWTTerm { variant : True, typ : Bool },
                Rule::r#false       => RazorWTTerm { variant : False, typ : Bool },
                _    => unreachable!("Rule : {:?} at Pos : {:?}", pair.as_rule(), pair.as_span()),
            }
        }
    
        Ok(parse_value(program))
    }
    
    pub fn eval_term(t : RazorWTTerm) -> RazorWTTerm
    {
        use RazorWTVariant::*;
        use RazorWTType::*;
    
        match &t.variant {
            IfThenElse { cond, lb, rb } =>  if Self::eval_term(*cond.clone()).variant == True { Self::eval_term(*lb.clone()) } else { Self::eval_term(*rb.clone()) },
            IsZero(u)                   =>  {   
                                                match Self::eval_term(*u.clone()).variant {
                                                    Num(n) => if n == 0 {RazorWTTerm {variant : True, typ : Bool}} else {RazorWTTerm {variant : False, typ : Bool}},
                                                    _ => unreachable!("IsZero called with non number term")
                                            }},
            Plus { lhs, rhs }           => {
                                                let mut sum = 0;
                                                match Self::eval_term(*lhs.clone()).variant {
                                                    Num(n) => sum += n,
                                                    _ => unreachable!("Plus called with non number term on left hand side")
                                                }
                                                match Self::eval_term(*rhs.clone()).variant {
                                                    Num(n) => sum += n,
                                                    _ => unreachable!("Plus called with non number term on right hand side")
                                                }
                                                RazorWTTerm {variant : Num(sum), typ : Nat} 
                                            },
            Num(n)                      => RazorWTTerm {variant : Num(*n), typ : Nat} ,
            True                        => RazorWTTerm {variant : True, typ : Bool} ,
            False                       => RazorWTTerm {variant : False, typ : Bool} ,
        }
    }
} 