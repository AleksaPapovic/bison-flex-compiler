//OPIS: inkrement u numexp-u
//RETURN: 7

int main() {

int a = 3;
int b = 4;
unsigned c = 3u;
int x = 5;
check (c){
when 3u =>

check (x){
when 3 =>
a = a + 5;
when 4 =>
{
b = 3;
}
default =>
a = a + b;
}

when 4u =>
{
b = 3;
}
default =>
a = a + b;
}

return a;

}

