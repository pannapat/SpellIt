import { Component } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  title = 'Angular';

  constructor(public translate: TranslateService) {
    translate.addLangs(['en', 'ga', 'th','it']);

    // this language will be used as a fallback when a translation isn't found in the current language
    translate.setDefaultLang('en');

    const browserLang = translate.getBrowserLang();
    // translate.use(browserLang.match(/en|ga/) ? browserLang : 'en');

    // the lang to use, if the lang isn't available, it will use the current loader to get them
    translate.use('en');
    // translate.use('ga');
    // translate.use('th');
  }
}
