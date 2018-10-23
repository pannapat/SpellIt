import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ParadigmAddComponent } from './paradigm-add.component';

describe('ParadigmAddComponent', () => {
  let component: ParadigmAddComponent;
  let fixture: ComponentFixture<ParadigmAddComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ParadigmAddComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ParadigmAddComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
