Imports System.Console
Imports System.Collections.Generic

Module bubble_sort
    Public Sub Print_range(Of SetType As IEnumerable)(Input_array As SetType)
        For Each element In Input_array
            Write(element)
            Write(" ")
        Next element
        WriteLine()
    End Sub

    Public Sub Bubble_sort(Of T As IComparable(Of T))(ByRef Input As IList(Of T))
        For it1 = 0 To Input.Count
            For it2 = 1 To Input.Count
                'these are unsorted! gonna swap 'em
                If Input(it2).CompareTo(Input(it2 - 1)) < 0 Then
                    Dim temp = Input(it2)
                    Input(it2) = Input(it2 - 1)
                    Input(it2 - 1) = temp
                End If
            Next it2
        Next it1
    End Sub

    Public Sub Main()
        Dim input As Double() = {1.0, 3, 2, 4, 5, 10, 50, 7, 1.5, 0.3}
        WriteLine("Before Sorting:")
        Print_range(input)
        Bubble_sort(input)
        WriteLine("After Sorting:")
        Print_range(input)
    End Sub
End Module
