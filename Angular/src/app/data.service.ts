import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class DataService {
  constructor(private http: HttpClient) {}

  getLanguageList() {
    // return this.http.post('https://localhost:9000/language-list', {});
    return this.http.get('https://jsonplaceholder.typicode.com/users');
    // return this.http.get('./app/mock-language-list.json');
  }
}
