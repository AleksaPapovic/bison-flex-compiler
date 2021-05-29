//OPIS: inkrement u numexp-u
//RETURN: 3

int main() {

int a = 3;
int b = 4;
unsigned c = 1u;
int x = 5;
check (c){
when 3u =>
a = 5;
when 4u =>
{
b = 3;
}
default =>
a = a ;
}


check (x){
when 3 =>
a = a + 5;
when 4 =>
{
b = 3;
}
default =>
a = a;
}

return a;

}
