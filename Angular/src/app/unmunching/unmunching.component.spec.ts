import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { UnmunchingComponent } from './unmunching.component';

describe('UnmunchingComponent', () => {
  let component: UnmunchingComponent;
  let fixture: ComponentFixture<UnmunchingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ UnmunchingComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UnmunchingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
