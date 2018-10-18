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
  slotRawList: string;
  constructor(private data: DataService, private router: Router, private location: Location) {}

  slots = [{ slot_id: 0 }];

  ngOnInit() {
  }

  addNewSlot() {
    this.slots.push({ slot_id: this.slots.length });
  }

  addNewParadigm() {
    this.data
      .addParadigm(this.paradigmName, this.slotRawList.split(","))
      .subscribe();
  }

  goBack(): void {
    this.location.back();
  }
}
