import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Location } from '@angular/common';
import { DataService } from '../data.service';


@Component({
  selector: 'app-paradigm-edit',
  templateUrl: './paradigm-edit.component.html',
  styleUrls: ['./paradigm-edit.component.scss']
})
export class ParadigmEditComponent implements OnInit {

  paradigm_name: string;
  paradigm$: Object;

  constructor(
  	private data: DataService,
  	private route: ActivatedRoute,
  	private location: Location
  	) { }

  ngOnInit() {
  	this.getParadigm();
  }

  getParadigm(): void{
  	const name = this.route.snapshot.paramMap.get('paradigm_name');
  	this.paradigm_name = name;
  	this.data.getParadigm(this.paradigm_name).subscribe(
      data => this.paradigm$ = data
    );

  }

  goBack(): void {
  this.location.back();
  }

}
