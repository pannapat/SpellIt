import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

//list of all available routes:
//api address: 104.248.116.233:5000

@Injectable({
  providedIn: 'root'
})
export class DataService {
  constructor(private http: HttpClient) {}

  getLanguageList() {
    return this.http.post('http://104.248.116.233:5000/language-list', {});
  }

  getParadigmList(language_name: string) {
    return this.http.post('http://104.248.116.233:5000/paradigm-list', {
      language_name: language_name
    });
  }

  getParadigmRoots(paradigm_name: string){
    return this.http.post('http://104.248.116.233:5000/root-word-list', {
      paradigm_name: paradigm_name
    });
  }

  getParadigmWords(paradigm_root: string){
  	return this.http.post('http://104.248.116.233:5000/word-form-list', {
  		paradigm_root: paradigm_root
  	});
  }

  getParadigmSlots(paradigm_name: string){
  	return this.http.post('http://104.248.116.233:5000/paradigm-slots', {
      paradigm_name: paradigm_name
    });
  }

  getAffix(language_name: string) {
    return this.http.post('http://104.248.116.233:5000/get-affix', {
      language_name: language_name
    });
  }

  addLanguage(language_name: string) {
    return this.http.post('http://104.248.116.233:5000/add-language', {
      language_name: language_name
    });
  }

  addParadigm(paradigm_name: string, slots: string[]) {
    return this.http.post('http://104.248.116.233:5000/add-paradigm', {
      paradigm_name: paradigm_name,
      slots: slots
    });
  }

  addParadigmWords(root_word: string, words: Object) {
    return this.http.post('http://104.248.116.233:5000/add-paradigm-words', {
      root_word: root_word,
      words: words
    });
  }

  //start of delete calls
  deleteWord(root_word: string) {
    return this.http.post('http://104.248.116.233:5000/delete-paradigm-word', {
      root_word: root_word
    });
  }

  deleteParadigm(paradigm_name: string){
    return this.http.post('http://104.248.116.233:5000/delete-paradigm', {
      paradigm_name: paradigm_name
    });
  }

  deleteLanguage(language_name: string){
    return this.http.post('http://104.248.116.233:5000/delete-language', {
      language_name: language_name
    });
  }

}
