namespace Pedro.Limits.SetForm

module Mixed = 
    // 3. 
    // Stst types utilize Oo types in their recursive component, thus mixing 
    // Stst and Oo.
    // It can be traversed via inheritance, or pattern matching on non-terminal 
    // nodes.
    // Only terminal nodes are either Oo or Stst, and this dictates how they can 
    // be reached.

    type Oset<'t> (elems: List<Sset<'t>>) = 
        member this.elems = elems
          
    and SetElement<'t> (x: option<'t>) = 
        inherit Oset<'t>([])
        member this.x = x

    // Admits Sets and "Urelements", or Tuples via inheritance

    and Sset<'t> = T of option<'t> | S of Oset<'t>
    
    // Unlike Stst, Stuples are Sets. Tuples differ from Sets in their 
    // cardinality.

    type Otuple2<'t>(a: Sset<'t>, b: Sset<'t>) = 
        inherit Oset<'t>([
            a; 
            Sset.S(Oset<'t>([
                a; 
                b
            ]))
        ])
        member this.a = a
        member this.b = b
        
    type Otuple3<'t>(a: Sset<'t>, b: Sset<'t>, c: Sset<'t>) = 
        inherit Otuple2<'t>(
            Sset.S(Otuple2<'t>(
                a, 
                b
            )), 
            c
        )
                    
    and Otuple4<'t>(a: Sset<'t>, b: Sset<'t>, c: Sset<'t>, d: Sset<'t>) = 
        inherit Otuple2<'t>(
            Sset.S(Otuple2<'t>(
                Sset.S(Otuple2<'t>(
                    a, 
                    b
                )), 
                c
            )), 
            d
        )
        
    // Hitting language limitations.

    // Stuple2.T isn't castable into Sset2.T, so Stuple2.T can't be used. 
    // Sset2.T is the Stst terminus.
    
    // Stuple2.S can only be used as an envelope type to an Otuple hierarchy. 
    // Inner Tuples are Ssets, so give up Stuples for Stst.

    // Admits any size Otuple

    // and Stuple2<'t> = T of option<'t> | S of Otuple2<'t>

    // Unifying definition of a single Stst type for Sets, Tuples, or others, 
    // for kicks?
    
    // The unified Stst differs from Sset only by being specific about its 
    // terminus component as a generic type. But as Otuple and others should be 
    // child to Sset, Stst should be a Sset for genericity instead, undoing the 
    // unification.
    
    // If a "variance" happened over S 'otype wouldn't need to be cast.

    //type Stst<'otype, 't> = T of option<'t> | S of 'otype

    // subtypes as precisely 'otype.
    
    //and OsetUnf<'t> (elems: List<Stst<OsetUnf<'t>, 't>>) = 
    //    member this.elems = elems
            
    //type SetElementUnf<'t> (x: option<'t>) = 
    //    inherit OsetUnf<'t>([])
    //    member this.x = x

    // Going forward, only Osets or Otuples are defined, with Otuples being 
    // Osets and either of the two being Ssets.

    let construct() = 
        // Terminus with two Stst

        Sset.S(Oset<int>([
            Sset.T(None); 
            Sset.T(Some(1))
        ])) |> ignore
        
        // Terminus with two Oo

        Sset.S(Oset<int>([
            Sset.S(SetElement(None)); 
            Sset.S(SetElement(Some(1)))
        ])) |> ignore
        
        // Terminus with one Stst and one Oo

        Sset.S(Oset<int>([
            Sset.S(SetElement(None)); 
            Sset.T(Some(1))
        ])) |> ignore

        Sset.S(Oset<int>([
            Sset.S(SetElement(None)); 
            Sset.S(Oset<int>([
                Sset.S(SetElement(Some(1)))
            ]))
        ])) |> ignore
        
        Sset.S(Oset<int>([
            Sset.T(None); 
            Sset.S(Oset<int>([
                Sset.T(None)
            ]))
        ])) |> ignore

        Oset<int>([
            Sset.T(None)
        ]) |> ignore

        Otuple2<int>(
            Sset.T(None), 
            Sset.T(None)
        ) |> ignore
        
        Sset.S(Otuple2<int>(
            Sset.T(None), 
            Sset.T(None)
        )) |> ignore
        
        Sset.S(Otuple2<int>(
            Sset.S(Oset<int>([Sset.T(None)])), 
            Sset.T(None)
        )) |> ignore
        
        Sset.S(Otuple2<int>(
            Sset.S(Otuple2<int>(
                Sset.T(None), 
                Sset.S(Oset<int>([Sset.T(None)]))
            )), 
            Sset.T(None)
        )) |> ignore

        // Complex construction

        let _: Sset<int> = 
            Sset.S(Otuple2<int>(
                Sset.S(Otuple2<int>(
                    Sset.T(Some(1)), 
                    Sset.T(Some(2))
                )), 
                Sset.S(Otuple2<int>(
                    Sset.S(Otuple2<int>(
                        Sset.T(Some(3)), 
                        Sset.T(Some(4))
                    )), 
                    Sset.T(Some(4))
                ))
            ))

        ()

    let print() = 
        ()