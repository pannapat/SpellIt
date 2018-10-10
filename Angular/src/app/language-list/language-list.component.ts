import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { Observable } from 'rxjs';
import { MatFormFieldControl } from '@angular/material';

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
      data =>
        (this.languages$ = [
          {
            language_id: 'en_US',
            language_name: 'English'
          },
          {
            language_id: 'it_IT',
            language_name: 'Italian'
          },
          {
            language_id: 'ga_IE',
            language_name: 'Irish'
          },
          {
            language_id: 'th_TH',
            language_name: 'Thai'
          }
        ])
    );
  }
}
