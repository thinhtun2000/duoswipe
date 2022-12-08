import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SimpleAppLayoutComponent } from './simple-app-layout.component';

describe('SimpleAppLayoutComponent', () => {
  let component: SimpleAppLayoutComponent;
  let fixture: ComponentFixture<SimpleAppLayoutComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SimpleAppLayoutComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SimpleAppLayoutComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
