import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ParadigmEditComponent } from './paradigm-edit.component';

describe('ParadigmEditComponent', () => {
  let component: ParadigmEditComponent;
  let fixture: ComponentFixture<ParadigmEditComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ParadigmEditComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ParadigmEditComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
