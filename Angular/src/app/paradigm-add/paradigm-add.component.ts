import { Component, OnInit } from "@angular/core";
import { DataService } from "../data.service";
import { Router } from "@angular/router";

@Component({
  selector: "app-paradigm-add",
  templateUrl: "./paradigm-add.component.html",
  styleUrls: ["./paradigm-add.component.scss"]
})
export class ParadigmAddComponent implements OnInit {
  language_id: string;
  paradigmName: string;
  slotRawList: string;
  constructor(private data: DataService, private router: Router) {}

  slots = [{ slot_id: 0 }];

  ngOnInit() {
    //alert(this.language_id);
  }

  addNewSlot() {
    this.slots.push({ slot_id: this.slots.length });
  }

  addNewParadigm() {
    this.data
      .addParadigm(this.paradigmName, this.slotRawList.split(","))
      .subscribe();
    // this.router.navigate("..");
  }
}
