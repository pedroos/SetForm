namespace Pslab.Limits.SetForm

module Stst = 
    // 2. 
    // Sum-type form of Sets ("successor-terminus")
    
    // Stst Set

    type SetSucc<'t> (elems: List<Sset<'t>>) = 
        member this.elems = elems

    and Sset<'t> = T of option<'t> | S of SetSucc<'t>

    // Stst Tuple. Not a Set.
    
    type TupleSucc<'t> (a: Stuple<'t>, b: Stuple<'t>) = 
        member this.a = a
        member this.b = b
    
    and Stuple<'t> = T of option<'t> | S of TupleSucc<'t>
    
    // Ssets and Stuples differ in their elements' cardinality of infinite and 
    // finite

    // Stuples and Otuples differ by Stuple not being a Set and in their 
    // cardinality, where infinitely many Stuple sizes exist and finitely many 
    // Otuple sizes exist

    let construct() = 
        let _: Sset<int> = 
            Sset.S(SetSucc<int>([
                Sset.T(Some(0)); 
                Sset.T(Some(1))
            ]))
        
        let s = 
            Sset.S(SetSucc<int>([
                Sset.S(SetSucc<int>([
                    Sset.T(Some(0))
                ])); 
                Sset.S(SetSucc<int>([
                    Sset.T(Some(1))
                ])); 
                Sset.T(Some(2))
            ]))
        
        let s = 
            Sset.S(SetSucc<int>([
                Sset.S(SetSucc<int>([
                    Sset.T(Some(0))
                ])); 
                Sset.S(SetSucc<int>([
                    Sset.T(Some(1))
                ])); 
                Sset.T(Some(2))
            ]))

        Sset.S(SetSucc<int>([
            Sset.T(None); 
            Sset.T(None)
        ])) |> ignore

        // Stuples of size 1 are a "terminus"

        let _: Stuple<int> = 
            Stuple.T(Some(1))

        // Stuples of size 2+ are "successors"

        let _: Stuple<int> = 
            Stuple.S(TupleSucc<int>(
                Stuple.T(Some(1)), 
                Stuple.T(Some(2))
            ))

        // Stuples of arbitrary size can be formed
        
        let _: Stuple<int> = 
            Stuple.S(TupleSucc<int>(
                Stuple.S(TupleSucc<int>(
                    Stuple.T(Some(1)), 
                    Stuple.T(Some(2))
                )), 
                Stuple.T(Some(3))
            ))
        
        // Tuples with irregular hierarchies can be formed. To inspect this, we 
        // need to print out our tree.
                
        let _: Stuple<int> = 
            Stuple.S(TupleSucc<int>(
                Stuple.S(TupleSucc<int>(
                    Stuple.T(Some(1)), 
                    Stuple.T(Some(2))
                )), 
                Stuple.S(TupleSucc<int>(
                    Stuple.S(TupleSucc<int>(
                        Stuple.T(Some(3)), 
                        Stuple.T(Some(4))
                    )), 
                    Stuple.T(Some(4))
                ))
            ))
            
        ()

    let print() = 
        ()