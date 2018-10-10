import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { Observable } from 'rxjs';

@Component({
  selector: 'app-paradigm-list',
  templateUrl: './paradigm-list.component.html',
  styleUrls: ['./paradigm-list.component.scss']
})
export class ParadigmListComponent implements OnInit {
  paradigms$: Object;

  constructor(private data: DataService) { }

  ngOnInit() {
    this.data.getParadigmList().subscribe(
      data =>
        (this.paradigms$ = [
          {
            paradigm_id: 'verb',
            paradigm_name: 'Verb'
          },
          {
            paradigm_id: 'noun',
            paradigm_name: 'Noun'
          },
          {
            paradigm_id: 'verb-ar',
            paradigm_name: 'Verb ar'
          },
          {
            paradigm_id: 'verb-xy',
            paradigm_name: 'Verb xy'
          }
        ])
    );
  }

}
