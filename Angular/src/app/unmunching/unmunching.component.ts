import { Component, OnInit } from '@angular/core';
import { DataService } from '../data.service';
import { ActivatedRoute } from '@angular/router';
import { Location } from '@angular/common';

@Component({
  selector: 'app-unmunching',
  templateUrl: './unmunching.component.html',
  styleUrls: ['./unmunching.component.scss']
})
export class UnmunchingComponent implements OnInit {
  unmunchedData$: Object
  check$: Object
  show$: Object
  showList: Boolean

  constructor(
  	private data: DataService,
  	private route: ActivatedRoute,
  	private location: Location) { }

  ngOnInit() {
    this.check$ = [];
    this.show$ = [];
    this.showList = false;
  }

  goBack(): void {
    this.location.back();
  }

  showForms(id) {
    this.show$[id] = !this.show$[id];
  }

  /**
   * this function send the WordList to the backend
   * and receive the processed data
   */
  uploadWordList() {
    this.unmunchedData$ = this.data.getUnmunch();
    this.showList = true;
  }

  /**
   * this function send the selected data to the backend 
   */
  addWords() {

  }

}
