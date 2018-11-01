import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { Location } from '@angular/common';

@Component({
  selector: 'app-language-add',
  templateUrl: './language-add.component.html',
  styleUrls: ['./language-add.component.scss']
})
export class LanguageAddComponent implements OnInit {
  languageName: string;
  status: Object;

  constructor(
    private data: DataService,
    private location: Location
    ) { }

  ngOnInit() {
  }

  addLanguage(){
  	this.data.addLanguage(this.languageName).subscribe(
      data => this.status = data
    );
  }

  goBack(): void {
    this.location.back();
  }
}
