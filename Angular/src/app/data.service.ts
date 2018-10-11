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

  getParadigmList() {
    return this.http.post('http://localhost:8080/paradigm-list', {});
  }

  getParadigm(){
  	return this.http.post('http://localhost:8080/paradigm', {});
  }
}
