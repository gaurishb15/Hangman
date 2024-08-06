#include <bits/stdc++.h>
#define mat(t) vector<vector<t>>
using namespace std;
#define n_char 28
int mem1[n_char] = {0};
int mem2[n_char][n_char] = {0};
int mem3[n_char][n_char][n_char] = {0};
int mem4[n_char][n_char][n_char][n_char] = {0};
int mem5[n_char][n_char][n_char][n_char][n_char] = {0};


void add(vector<double> & a, vector<double> b){
    for(int i = 0; i<a.size(); i++){
        a[i] += b[i];
    }
}
string replace(string word, char x){
    for(int i = 0; i<word.size(); i++){
        if(word[i] == '-'){
            word[i] = x;
        }
    }
    return word;
}
vector<double> normalize(vector<double> m, vector<bool>& not_possible){
    double sum = 0;
    for(int i = 0; i<m.size(); i++){
        if(not_possible[i])
            m[i]  = 0;
        else sum += m[i];
    }
    if(sum>1){
        for(int i = 0; i<m.size(); i++){
            m[i] /= sum;
        }
        return m;
    }
    sum = 0;
    for(int i = 0; i<n_char; i++){
        if(!not_possible[i]){
            m[i] = mem1[i];
            sum += mem1[i];
        }
    }
    for(int i = 0; i<n_char; i++){
        m[i] = m[i]/sum;
    }
    return m;
}
char find_best_letter(vector<double> & score, vector<bool>& not_possible){
    int r = -1; double m = -1;
    for (int i = 0; i < n_char; i++){
        if (score[i] > m && !not_possible[i]){
            m = score[i];
            r = i;
        }
    }
    if(r == -1){
        printf("\n---error---\n");
        throw std::runtime_error("error");
        return 'a';
    }
    return 'a' + r;
}
int number_of_blanks(string p){
    int k = 0;
    for(int i = 0; i<p.size(); i++){
        if(p[i] == '-'){
            k++;
        }
    }
    return k;
}


vector<double> Conditonal_Probability(string pattern, vector<bool>& not_possible){
    vector<double> probability(n_char, 0.0);
    if(pattern.size() == 2){
      for(int i = 0; i<n_char; i++){
        if(!not_possible[i]){
            string p = replace(pattern, i+'a');
            probability[i] = probability[i] + mem2[p[0]-'a'][p[1]-'a'];
        }
      }
    } 
    else if(pattern.size() == 3){
      for(int i = 0; i<n_char; i++){
        if(!not_possible[i]){
            string p = replace(pattern, i+'a');
            probability[i] = probability[i] + mem3[p[0]-'a'][p[1]-'a'][p[2]-'a'];
        }
      }
    } 
    else if(pattern.size() == 4){
      for(int i = 0; i<n_char; i++){
        if(!not_possible[i]){
            string p = replace(pattern, i+'a');
            probability[i] = probability[i] + mem4[p[0]-'a'][p[1]-'a'][p[2]-'a'][p[3]-'a'];
        }
      }
    } 
    else if(pattern.size() == 5){
      for(int i = 0; i<n_char; i++){
        if(!not_possible[i]){
            string p = replace(pattern, i+'a');
            probability[i] = probability[i] + mem5[p[0]-'a'][p[1]-'a'][p[2]-'a'][p[3]-'a'][p[4]-'a'];
        }
      }
    } 
    return normalize(probability, not_possible);
}

vector<double> model(string &word, vector<bool>& not_possible, int length){
    vector<double> score(n_char, 0.0);
    vector<string> patterns;
    for (int i = 0; i + length - 1 < word.size(); i++)
    {
        if(number_of_blanks(word.substr(i, length)) == 1){
            patterns.push_back(word.substr(i, length));
        }
    }
    for(string p : patterns){
        add(score, Conditonal_Probability(p, not_possible));
    }
    return score;

}

char guess(string word, vector<bool>& not_possible, int  order = 5){
    double coeff[] = {1, 1, 4, 10, 20};
    vector<vector<double>> model_results(order);
    for(int i =0; i<order; i++){
        model_results[i] = model(word, not_possible, i+1);
    }
    vector<double> score(n_char, 0);
    for(int j = 0; j<order; j++){
        for(int i = 0; i<n_char; i++){
            score[i] += model_results[j][i] * coeff[j];
        }
    }
    return find_best_letter(score, not_possible);
}

int play(string word, bool show_game = false){
    vector<bool> not_possible(n_char, false);
    not_possible[n_char-1] = not_possible[n_char-2] = true;
    string input(word.size(), '-');
    input = "{" + input + "|";
    bool done = false;
    int wrong_attempts = 0;
    while(!done){
        char res = guess(input, not_possible);
        
        if(show_game)
            cout << input << "\t" << res; 

        bool found = false;
        done = true;
        for(int i = 0; i<word.size(); i++){
            if(res == word[i]){
                input[i+1] = res;
                found = true;
            }
            if(input[i] == '-'){
                done = false;
            }
        }
        not_possible[res-'a'] = true;
        if(!found){
            wrong_attempts++;
            if(show_game){
                cout << " X";
            }
        }
        if(show_game){
            cout << endl;
        }
    }
    return wrong_attempts;

}
vector<string> create_dictionary(string path){
    ifstream MyFile(path);
    string s;
    vector<string> dictionary;
    for (int i = 0; getline(MyFile, s); i++)
    {
        dictionary.push_back(s);
    }
    MyFile.close();
    return dictionary;
}
void delete_model(){
    for(int i =0; i<n_char; i++){
        mem1[i] = 0;
        for(int j = 0; j<n_char; j++){
            mem2[i][j] = 0;
            for(int k= 0; k<n_char; k++){
                mem3[i][j][k] = 0;
                for(int l = 0; l<n_char; l++){
                    mem4[i][j][k][l] = 0;
                    for(int m = 0; m<n_char; m++){
                        mem5[i][j][k][l][m];
                    }
                }
            }
        }

    }
}
void create_model(vector<string> dictionary){
    for(string &word : dictionary){
        word = "{" + word + "|";
    }
    // model 1
    for (int i = 0; i < dictionary.size(); i++)
    {
        bool unique_letters[n_char] = {false};
        for (int j = 0; j < dictionary[i].size(); j++)
        {
            unique_letters[dictionary[i][j] -'a'] = true;
        }
        for(int j = 0; j<n_char; j++){
            if(unique_letters[j]){
                mem1[j]++; 
            }
        }
    }
    // model 2
    for (int i = 0; i < dictionary.size(); i++)
    {
        for (int j = 0; j + 1 < dictionary[i].size(); j++)
        {
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            mem2[a][b]++;
        }
    }

    // model3
    for(int i = 0; i<dictionary.size();i++){
        for(int j = 0; j+2 < dictionary[i].size(); j++){
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            int c = dictionary[i][j+2] - 'a';
            mem3[a][b][c]++;
        }
    }
    // model4
    for(int i = 0; i<dictionary.size();i++){
        for(int j = 0; j+3 < dictionary[i].size(); j++){
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            int c = dictionary[i][j+2] - 'a';
            int d = dictionary[i][j+3] - 'a';
            mem4[a][b][c][d]++;
        }
    }
    // model5
    for(int i = 0; i<dictionary.size();i++){
        for(int j = 0; j+4 < dictionary[i].size(); j++){
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            int c = dictionary[i][j+2] - 'a';
            int d = dictionary[i][j+3] - 'a';
            int e = dictionary[i][j+4] - 'a';
            mem5[a][b][c][d][e]++;
        }
    }

}

pair<vector<string>, vector<string>> train_test_split(vector<string>& dictionary, int test_size){
    vector<bool> in_test_set(dictionary.size(), false);
    pair<vector<string>, vector<string>> res;
    while(test_size){
        int ind = ((rand()%10000)*10000  + rand()%10000) % dictionary.size();
        if(!in_test_set[ind]){
            in_test_set[ind] = true;
            test_size--;
        }
    }
    for(int i = 0; i<dictionary.size(); i++){
        if(in_test_set[i]){
            res.second.push_back(dictionary[i]);
        }
        else res.first.push_back(dictionary[i]);
    }
    return res;
}
int main()
{
    srand(43);
    vector<string> dictionary = create_dictionary("words_250000_train.txt");
    //  create_model(dictionary);
    //  cout << play("pixel", true);

    // Cross Validation
    for(int i = 0; i<10;i++){
    int n = 1000;
    pair<vector<string>, vector<string>> dict = train_test_split(dictionary, n);
    create_model(dict.first);
    int success = 0;
    for(int i = 0; i<n; i++){
        if(play(dict.second[i]) < 6){
            success++;
        }
        //cout << dict.second[i].size() << "," << play(dict.second[i])<< ",";
        //else cout << play(dict.second[i], true)<<endl;
    }
    double temp  = success;
    cout << temp/n << ", ";
    delete_model();
    }
    return 0;
}