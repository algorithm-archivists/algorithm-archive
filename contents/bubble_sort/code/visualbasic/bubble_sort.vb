Imports System

Module Module1
    Public Sub Print_range(Input_array() As Double)
        For Each element In Input_array
            Console.WriteLine(element)
        Next element
    End Sub

    Public Sub Bubble_sort(ByRef Input_array() As Double)
        For it1 = 0 To Input_array.Length
            For it2 = 1 To Input_array.Length - 1
                'these are unsorted! gonna swap 'em
                If (Input_array(it2) < Input_array(it2 - 1)) Then
                    'Swaping
                    Dim temp As Double
                    temp = Input_array(it2)
                    Input_array(it2) = Input_array(it2 - 1)
                    Input_array(it2 - 1) = temp
                End If
            Next it2
        Next it1
    End Sub

    Public Sub Main()
        Dim input() As Double = {1.0, 3, 2, 4, 5, 10, 50, 7, 1.5, 0.3}
        'Using 1.0 since 1. is unable to read
        Console.WriteLine("Before Sorting:")
        Print_range(input)

        Bubble_sort(input)

        Console.WriteLine("After Sorting:")
        Print_range(input)
    End Sub
End Module
