//OPIS: inkrement u numexp-u
//RETURN: 6

int main() {

int a = 3;
int b = 4;
unsigned c = 3u;

check (c){
when 3u =>
 a=6;
when 4u =>
{
b = 5;
}
default =>
a = a + b;
}

return a;

}
