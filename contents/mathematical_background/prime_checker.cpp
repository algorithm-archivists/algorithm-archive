//This is program to check if a given number is a prime number with run time of O(sqrt(n))

#include<iostream>
#include<math.h>

using namespace std;

bool isPrime(long int a)
{
    int flag=1;
    for(long int i=2;i<=sqrt(a);i++)
    {
        if(a%i==0)
		{
			flag=0;
			break;
    	}
    }
    if(flag == 1)
        return(true);
    else
        return(false);
}
 int main()
 {
    cout << "enter the number to be checked" << endl;
    int n;
    cin >> n ;
    if(isPrime(n))
        cout << "prime" << endl;
    else
    {
        cout << "composite" << endl;
    }
    
     return 0;
 }