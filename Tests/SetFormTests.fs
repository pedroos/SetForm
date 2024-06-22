namespace Pslab.Limits.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Pslab.Limits.SetForm.MixedRename

// Test some basic constructions

[<TestClass>]
type SetFormTests () = 
    [<TestMethod>]
    member this.MixedTest1() = 
        let r: Rec<int> = 
            Su(Pair<int>(
                Su(Pair<int>(
                    Te(Some(1)), 
                    Te(Some(2))
                )), 
                Su(Pair<int>(
                    Su(Pair<int>(
                        Te(Some(3)), 
                        Te(Some(4))
                    )), 
                    Te(Some(4))
                ))
            ))

        let v: int = 
            match r with Su su -> 
                match su.elems.Head with Su su -> 
                    match su.elems.Head with Te te -> te.Value
        
        Assert.AreEqual(1, v)
        
    [<TestMethod>]
    member this.MixedTest1b() = 
        let r: Rec<int> = 
            Su(Pair<int>(
                Su(Pair<int>(
                    Te(Some(1)), 
                    Te(Some(2))
                )), 
                Su(Pair<int>(
                    Su(Pair<int>(
                        Te(Some(3)), 
                        Te(Some(4))
                    )), 
                    Te(Some(5))
                ))
            ))

        // Traversing as Sets. Pair elements would be preferrable to Set 
        // elements. Set internal representation subject to change.

        // Uneeded complete pattern form

        //let v: option<int> = 
        //    match r with 
        //    | Su su -> 
        //        match su.elems.Item(1) with 
        //        | Su su -> 
        //            match su.elems.Head with
        //            | Su su -> 
        //                match su.elems.Item(1) with 
        //                | Su _ -> None
        //                | Te te -> Some(te.Value)
        //            | Te te -> Some(te.Value)
        //        | Te te -> Some(te.Value)
        //    | Te te -> Some(te.Value)

        let v: int = 
            match r with Su su -> 
                match su.elems.Item(1) with Su su -> 
                    match su.elems.Head with Su su -> 
                        match su.elems.Item(1) with Te te -> te.Value
        
        Assert.AreEqual(4, v)
        
    [<TestMethod>]
    member this.MixedTest1c() = 
        let r: Rec<int> = 
            Su(Pair<int>(
                Su(Pair<int>(
                    Te(Some(1)), 
                    Te(Some(2))
                )), 
                Su(Pair<int>(
                    Su(Pair<int>(
                        Te(Some(3)), 
                        Te(Some(4))
                    )), 
                    Te(Some(5))
                ))
            ))

        // Traversing as Pairs. Pair > Set cast due to covariance issue.
        // For example, incorrectly casting a Pair as a Tuple3 would fail at 
        // runtime.

        let v: int = 
            match r with Su su -> 
                match (su :?> Pair<int>).b with Su su -> 
                    match (su :?> Pair<int>).a with Su su -> 
                        match (su :?> Pair<int>).b with Te te -> te.Value
        
        Assert.AreEqual(4, v)