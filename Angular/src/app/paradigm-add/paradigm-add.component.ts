import { Component, OnInit } from "@angular/core";
import { DataService } from "../data.service";
import { Router } from "@angular/router";
import { Location } from '@angular/common';

@Component({
  selector: "app-paradigm-add",
  templateUrl: "./paradigm-add.component.html",
  styleUrls: ["./paradigm-add.component.scss"]
})
export class ParadigmAddComponent implements OnInit {
  language_id: string;
  paradigmName: string;
  slots: string[] = ['root'];
  newSlot: string;
  constructor(
    private data: DataService, 
    private router: Router, 
    private location: Location
    ) {}

  ngOnInit() {
  }

  addNewParadigm() {
    if (this.newSlot !== '') {
      this.addNewSlot(this.newSlot);
    }
    this.data
      .addParadigm(this.paradigmName, this.slots)
      .subscribe();
  }

  goBack(): void {
    this.location.back();
  }

  onKeydown(event, newSlot) {
    if (event.key === 'Enter') {
      this.addNewSlot(newSlot);
    }
  }

  addNewSlot(newSlot: string) {
    this.slots.push(newSlot);
    this.newSlot = '';
  }
}
