import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { ActivatedRoute } from '@angular/router';
import { Location } from '@angular/common';
import { Observable } from 'rxjs';

@Component({
  selector: 'app-paradigm-list',
  templateUrl: './paradigm-list.component.html',
  styleUrls: ['./paradigm-list.component.scss']
})
export class ParadigmListComponent implements OnInit {
  paradigms$: Object;
  language_name: string;

  constructor(
  	private data: DataService,
  	private route: ActivatedRoute,
  	private location: Location) { }

  ngOnInit() {
  	const name = this.route.snapshot.paramMap.get('language_name');
    this.language_name = name;
    this.data.getParadigmList(name).subscribe(
      data =>
        (this.paradigms$ = data["paradigms"])
    );
  }

  goBack(): void {
    this.location.back();
  }

  deleteLanguage(){
    if (confirm("Are you sure you want to delete language: " + this.language_name + "?")){
      this.data.deleteLanguage(this.language_name).subscribe();
      this.location.back();
    }
  }

  exportAffixFile() {
    alert("Download may take a few seconds")
    this.data.getAffix(this.language_name).subscribe(data => {
      var fileContents= data;
      this.saveTextAsFile(fileContents, this.language_name + ".aff");
      });  
  }

  saveTextAsFile (data, filename){
    if(!data) {
            console.error('No data to save')
            return;
        }

        if(!filename) filename = 'affix.aff'

        var blob = new Blob([data], {type: 'text/plain'}),
            e    = document.createEvent('MouseEvents'),
            a    = document.createElement('a')

  if (window.navigator && window.navigator.msSaveOrOpenBlob) {
      window.navigator.msSaveOrOpenBlob(blob, filename);
  }
  else{
      var e = document.createEvent('MouseEvents'),
          a = document.createElement('a');

      a.download = filename;
      a.href = window.URL.createObjectURL(blob);
      a.dataset.downloadurl = ['text/plain', a.download, a.href].join(':');
      e.initEvent('click', false, false);
      a.dispatchEvent(e);
  }
  }

}
