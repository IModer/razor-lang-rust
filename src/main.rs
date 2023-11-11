#![allow(uncommon_codepoints)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)]

use pest::Parser;

//pub mod json;  // We declare we have a module with name 'json' in json/mod.rs
//pub mod repl;
pub mod razor;
///pub mod lambda;

fn main() -> std::io::Result<()>  {
    /*

    use json::*;

    let unparsed_file = fs::read_to_string("data.json").expect("cannot read file");

    let json: JSONValue = parse_json_file(&unparsed_file).expect("unsuccessful parse");

    println!("{}", serialize_jsonvalue(&json));
    */
    
    /*
    use repl::*;
    use repl::Op::*;
    use repl::Expr::BinOp;
    use repl::Expr::*;
    let val = BinOp {   lhs: ( Box::new(Integer(10)) ), 
                        op:  ( Lte ), 
                        rhs: ( Box::new(BinOp {     lhs: ( Box::new(Integer(10)) ), 
                                                    op:  ( Times ), 
                                                    rhs: ( Box::new(Integer(20)) ) })
                             ) 
                    };
    */

    //println!("10 <= 20 : {}" , serialize_expression(&val));

    /*
    let mut buffer = String::new();
    let stdin = std::io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer)?;

    let repl = ReplParser::parse(Rule::program, &buffer).expect("parsing failed").next().unwrap();

    for a in repl.into_inner() {
        print!("{}", a);
    }
    */

    use razor::RazorASTTerm;

    let t = RazorASTTerm::IfThenElse { cond: Box::new(RazorASTTerm::IsZero(Box::new(RazorASTTerm::Num(10)))), 
                                         lb: Box::new(RazorASTTerm::Plus {  lhs: Box::new(RazorASTTerm::Num(1)),
                                                                            rhs: Box::new(RazorASTTerm::Num(2)) }), 
                                         rb: Box::new(RazorASTTerm::Num(0)) };

    println!("RazorATS : ");

    println!("if isZero num 10 then num 1 + num 2 else num 2 :: {}", t);

    let a = RazorASTTerm::parse_term("if isZero ( if false then num 0 else num 2 ) then num 1 + num 2 else num 2".to_string()).unwrap();

    println!("{}", a);

    println!("{}", RazorASTTerm::eval_term(a));

    use razor::RazorWTTerm;

    println!("RazorWT : ");

    //if isZero num 0 
    //  then num 1 + num 2 
    //  else num 2
    let b = RazorWTTerm::parse_term("if isZero num 0 then num 1 + num 2 else num 2".to_string()).unwrap();

    println!("{}", b);

    println!("{}", RazorWTTerm::eval_term(b));

    Ok(())
}
