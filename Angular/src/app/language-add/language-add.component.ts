import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';

@Component({
  selector: 'app-language-add',
  templateUrl: './language-add.component.html',
  styleUrls: ['./language-add.component.scss']
})
export class LanguageAddComponent implements OnInit {
  languageName: string;
  status: Object;

  constructor(private data: DataService) { }

  ngOnInit() {
  }

  addLanguage(){
  	this.data.addLanguage(this.languageName).subscribe(
      data => this.status = data
    );
  }
  

}
