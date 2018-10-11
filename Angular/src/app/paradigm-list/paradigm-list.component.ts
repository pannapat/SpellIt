import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { Observable } from 'rxjs';

@Component({
  selector: 'app-paradigm-list',
  templateUrl: './paradigm-list.component.html',
  styleUrls: ['./paradigm-list.component.scss']
})
export class ParadigmListComponent implements OnInit {
  language_id: string;
  paradigms$: Object;

  constructor(private data: DataService) { }

  ngOnInit() {
    this.data.getParadigmList(this.language_id).subscribe(
      data =>
        (this.paradigms$ = data)
    );
  }

}
