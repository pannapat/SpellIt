import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-paradigm-add',
  templateUrl: './paradigm-add.component.html',
  styleUrls: ['./paradigm-add.component.scss']
})
export class ParadigmAddComponent implements OnInit {
  language_id:string
  constructor() { }

  slots = [
    {slot_id: 0}
  ]

  ngOnInit() {
    alert(this.language_id);    
  }

  addNewSlot() {
    this.slots.push( {slot_id: this.slots.length} )
  }
  

}
