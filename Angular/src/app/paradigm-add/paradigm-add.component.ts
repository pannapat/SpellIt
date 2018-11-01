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
  constructor(
    private data: DataService, 
    private router: Router, 
    private location: Location
    ) {}

  ngOnInit() {
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
