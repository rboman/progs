
#include <iostream>
int f1();
int f2();
int f3();

using namespace std;


struct RoBo
{
	RoBo( char *txt)
	{
		cout << "Error in " << txt << endl;
	}

};


int f1()
{
	try{
		f2();
	} catch(RoBo) {
		throw RoBo("f1");
	}


	return 0;
}

int f2()
{
	try{
		f3();
	} catch(RoBo) {
		throw RoBo("f2");
	}


	return 0;
}

int f3()
{
	throw RoBo("f3");

	return 0;
}


int main()
{
	try{
		f1();
	} catch(RoBo){
	}

	return 0;
}