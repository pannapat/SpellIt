import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class DataService {
  constructor(private http: HttpClient) {}

  getLanguageList() {
    return this.http.post('http://localhost:8080/language-list', {});
    // return this.http.get('https://jsonplaceholder.typicode.com/users');
    // return this.http.get('./app/mock-language-list.json');
  }

  getParadigmList(language_id: string) {
    return this.http.post('http://localhost:8080/paradigm-list', {
      'language_id': language_id
    });
  }

  getParadigm(paradigm_id: string) {
    return this.http.post('http://localhost:8080/paradigm', {
      'paradigm_id': paradigm_id
    });
  }
}
