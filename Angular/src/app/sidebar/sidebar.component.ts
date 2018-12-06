import { Component, OnInit } from "@angular/core";
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: "app-sidebar",
  templateUrl: "./sidebar.component.html",
  styleUrls: ["./sidebar.component.scss"]
})

export class SidebarComponent implements OnInit {
  constructor(public translate: TranslateService) {}

  ngOnInit() {}

  changeLanguageTo(locale: string) {
    locale = locale || 'en';
    this.translate.use(locale);
  }


  isCurrentLanguage(locale: string): boolean {
    return (this.translate.currentLang === locale);
  }
}
