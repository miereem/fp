module Module18Tests

open NUnit.Framework
open Module18

[<TestFixture>]
type MaxPathSumTests() =

    [<Test>]
    member this.``Empty triangle returns 0``() =
        let emptyTriangle = []
        Assert.AreEqual(0, maxPathSumTailWrapper emptyTriangle, "Tail recursion should return 0 for empty triangle")
        Assert.AreEqual(0, maxPathSumRecursiveWrapper emptyTriangle, "Recursive should return 0 for empty triangle")
        Assert.AreEqual(0, maxPathSumModular emptyTriangle, "Modular should return 0 for empty triangle")

    [<Test>]
    member this.``Single row triangle returns the only element``() =
        let singleRowTriangle = [[5]]
        Assert.AreEqual(5, maxPathSumTailWrapper singleRowTriangle, "Tail recursion should return 5")
        Assert.AreEqual(5, maxPathSumRecursiveWrapper singleRowTriangle, "Recursive should return 5")
        Assert.AreEqual(5, maxPathSumModular singleRowTriangle, "Modular should return 5")

    [<Test>]
    member this.``Small triangle returns correct sum``() =
        let smallTriangle = [
            [3]
            [7; 4]
            [2; 4; 6]
            [8; 5; 9; 3]
        ]
        Assert.AreEqual(23, maxPathSumTailWrapper smallTriangle, "Tail recursion should return 23")
        Assert.AreEqual(23, maxPathSumRecursiveWrapper smallTriangle, "Recursive should return 23")
        Assert.AreEqual(23, maxPathSumModular smallTriangle, "Modular should return 23")

    [<Test>]
    member this.``Large triangle returns correct sum``() =
        let largeTriangle = [
            [75]
            [95; 64]
            [17; 47; 82]
            [18; 35; 87; 10]
            [20; 4; 82; 47; 65]
            [19; 1; 23; 75; 3; 34]
            [88; 2; 77; 73; 7; 63; 67]
            [99; 65; 4; 28; 6; 16; 70; 92]
            [41; 41; 26; 56; 83; 40; 80; 70; 33]
            [41; 48; 72; 33; 47; 32; 37; 16; 94; 29]
            [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14]
            [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57]
            [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48]
            [63; 66; 4; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31]
            [4; 62; 98; 27; 23; 9; 70; 98; 73; 93; 38; 53; 60; 4; 23]
        ]
        Assert.AreEqual(1074, maxPathSumTailWrapper largeTriangle, "Tail recursion should return 1074")
        Assert.AreEqual(1074, maxPathSumRecursiveWrapper largeTriangle, "Recursive should return 1074")
        Assert.AreEqual(1074, maxPathSumModular largeTriangle, "Modular should return 1074")