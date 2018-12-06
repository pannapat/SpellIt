import { BrowserModule } from "@angular/platform-browser";
import { NgModule } from "@angular/core";

import { AppRoutingModule } from "./app-routing.module";
import { AppComponent } from "./app.component";
import { SidebarComponent } from "./sidebar/sidebar.component";
import { LanguageListComponent } from "./language-list/language-list.component";
import { ParadigmListComponent } from "./paradigm-list/paradigm-list.component";
import { LanguageAddComponent } from "./language-add/language-add.component";
import { ParadigmEditComponent } from "./paradigm-edit/paradigm-edit.component";
import { ParadigmAddComponent } from "./paradigm-add/paradigm-add.component";
import { HttpClient, HttpClientModule } from "@angular/common/http";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";

import { FormControl, FormsModule } from "@angular/forms";
import {
  MatButtonModule,
  MatCheckboxModule,
  MatFormFieldModule,
  MatOptionModule,
  MatSelectModule,
  MatFormFieldControl,
  MatInputModule,
  MatIconModule,
  MatMenuTrigger,
  MatMenuModule,
  MatCardModule
} from "@angular/material";
import { UnmunchingComponent } from './unmunching/unmunching.component';

import { TranslateModule, TranslateLoader } from '@ngx-translate/core';
import { TranslatePoHttpLoader } from '@biesbjerg/ngx-translate-po-http-loader';

export function createTranslateLoader(http: HttpClient) {
	return new TranslatePoHttpLoader(http, 'assets/i18n', '.po');
}

@NgModule({
  declarations: [
    AppComponent,
    SidebarComponent,
    LanguageListComponent,
    ParadigmListComponent,
    LanguageAddComponent,
    ParadigmEditComponent,
    ParadigmAddComponent,
    UnmunchingComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    HttpClientModule,
    BrowserAnimationsModule,
    MatButtonModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatOptionModule,
    MatSelectModule,
    MatInputModule,
    MatIconModule,
    FormsModule,
    MatMenuModule,
    MatCardModule,
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: createTranslateLoader,
        deps: [HttpClient]
      }
    })
  ],
  exports: [MatButtonModule, MatCheckboxModule, TranslateModule],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule {}
