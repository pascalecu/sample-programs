Imports System
Imports System.Collections.Generic
Imports System.Linq

Module RomanNumeral

    Private Function GetValue(c As Char) As Integer
        Select Case c
            Case "M"c : Return 1000
            Case "D"c : Return 500
            Case "C"c : Return 100
            Case "L"c : Return 50
            Case "X"c : Return 10
            Case "V"c : Return 5
            Case "I"c : Return 1
            Case Else
                Throw New ArgumentException("Invalid Roman numeral character")
        End Select
    End Function

    Private Function RomanToDecimal(roman As String) As Integer
        Dim total As Integer = 0
        Dim lastValue As Integer = 0

        For i As Integer = roman.Length - 1 To 0 Step -1
            Dim currentValue As Integer = GetValue(roman(i))

            If currentValue < lastValue Then
                total -= currentValue
            Else
                total += currentValue
            End If

            lastValue = currentValue
        Next

        Return total
    End Function

    Sub Main(args As String())
        If args.Length = 0 OrElse String.IsNullOrWhiteSpace(args(0)) Then
            Console.WriteLine("Usage: please provide a string of roman numerals")
            Return
        End If

        Try
            Console.WriteLine(RomanToDecimal(args(0).ToUpperInvariant()))
        Catch ex As ArgumentException
            Console.WriteLine("Error: invalid string of roman numerals")
        End Try
    End Sub

End Module