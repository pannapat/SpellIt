import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ParadigmListComponent } from './paradigm-list.component';

describe('ParadigmListComponent', () => {
  let component: ParadigmListComponent;
  let fixture: ComponentFixture<ParadigmListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ParadigmListComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ParadigmListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
