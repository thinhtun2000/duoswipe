import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SwipingComponent } from './swiping.component';

describe('SwipingComponent', () => {
  let component: SwipingComponent;
  let fixture: ComponentFixture<SwipingComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SwipingComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SwipingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
