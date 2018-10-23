import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { LanguageListComponent } from './language-list/language-list.component';
import { ParadigmListComponent } from './paradigm-list/paradigm-list.component';
import { LanguageAddComponent } from './language-add/language-add.component';
import { ParadigmEditComponent } from './paradigm-edit/paradigm-edit.component';
import { ParadigmAddComponent } from './paradigm-add/paradigm-add.component';

const routes: Routes = [
  {
    path: '',
    component: LanguageListComponent
  },
  {
    path: 'language-list',
    component: LanguageListComponent
  },
  {
    path: 'language/:language_name',
    component: ParadigmListComponent
  },
  {
    path: 'language/:language_name/paradigm-edit/:paradigm_name',
    component: ParadigmEditComponent
  },
  {
    path: 'language-add',
    component: LanguageAddComponent
  },
  {
    path: 'language/:language_name/paradigm-add',
    component: ParadigmAddComponent
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
