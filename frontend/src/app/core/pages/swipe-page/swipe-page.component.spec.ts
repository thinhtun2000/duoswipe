import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SwipePageComponent } from './swipe-page.component';

describe('SwipePageComponent', () => {
  let component: SwipePageComponent;
  let fixture: ComponentFixture<SwipePageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SwipePageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SwipePageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
