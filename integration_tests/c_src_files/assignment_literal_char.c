int main(void){
    char variable = 'a';
    int exit_code = 2;

    if (variable == 'a'){
        exit_code = 0;
    }
    else{
        exit_code = 1;
    }    
    
    return exit_code;
}