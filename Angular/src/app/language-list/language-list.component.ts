import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';

@Component({
  selector: 'app-language-list',
  templateUrl: './language-list.component.html',
  styleUrls: ['./language-list.component.scss']
})
export class LanguageListComponent implements OnInit {
  languages$: Object;


  constructor(private data: DataService) {}

  ngOnInit() {
    // Actually, we won't override the data from API but now we just need some mock data to display
    this.data.getLanguageList().subscribe(
      data => this.languages$ = data["languages"]
    );
  }
}
