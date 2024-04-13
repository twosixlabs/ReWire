#include <stdio.h>

int euclidsAlgorithm(int a, int b){

	if (b > a){
		return -1;
	} else if (b == a){
		return a;
	} else if (b == 0){
		return a;
	} else {
		return euclidsAlgorithm( b, (a % b));
	}
}

int main(void) {

	int x = 255;
	int y = 5;

	return euclidsAlgorithm(x, y);

}
