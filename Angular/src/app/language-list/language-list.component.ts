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
    this.data.getLanguageList().subscribe(
      data => this.languages$ = data["languages"]
    );
  }
}
