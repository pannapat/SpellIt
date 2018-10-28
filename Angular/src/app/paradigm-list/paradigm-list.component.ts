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
    this.data.getAffix(this.language_name).subscribe();
  }

}
