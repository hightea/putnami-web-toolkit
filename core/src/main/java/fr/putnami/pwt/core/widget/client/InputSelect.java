/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractInputSelect;

public class InputSelect<T> extends AbstractInputSelect<T, T> {

	private class SelectionHandler implements SelectItemSelectionHandler<T, T> {
		private T selectedItem;

		@Override
		public void onItemClick(T item) {
			this.setSelection(item, true);
			InputSelect.this.getDropdown().close();
		}

		@Override
		public void onHomeKeyDown() {
			this.selectByIndex(0);
		}

		@Override
		public void onEndKeyDown() {
			this.selectByIndex(InputSelect.this.getOrderedItems().size() - 1);
		}

		@Override
		public void onUpKeyDown() {
			this.selectByIndex(InputSelect.this.getOrderedItems().indexOf(this.selectedItem) - 1);
		}

		@Override
		public void onDownKeyDown() {
			this.selectByIndex(InputSelect.this.getOrderedItems().indexOf(this.selectedItem) + 1);
		}

		@Override
		public void onItemSearch(T searchResult) {
			this.setSelection(searchResult, true);
		}

		private void selectByIndex(int index) {
			int newIndex = Math.min(InputSelect.this.getOrderedItems().size() - 1, Math.max(0, index));
			this.setSelection(InputSelect.this.getOrderedItems().get(newIndex), true);
		}

		private void scrollToSelected() {
			NavLink currentSelection = InputSelect.this.getItemsLinks().get(this.selectedItem);
			if (InputSelect.this.getDropdown().isOpen() && currentSelection != null) {
				currentSelection.getElement().scrollIntoView();
			}
		}

		@Override
		public void setSelection(T selection, boolean fireEvents) {
			T oldValue = this.selectedItem;
			NavLink currentSelection = InputSelect.this.getItemsLinks().get(this.selectedItem);
			if (currentSelection != null) {
				currentSelection.setActive(false);
			}
			NavLink newSelection = InputSelect.this.getItemsLinks().get(selection);
			if (newSelection != null) {
				newSelection.setActive(true);
			}
			this.selectedItem = selection;
			this.scrollToSelected();
			if (fireEvents) {
				ValueChangeEvent.fireIfNotEqual(InputSelect.this, oldValue, selection);
			}
		}

		@Override
		public T getSelection() {
			return this.selectedItem;
		}
	}

	public InputSelect() {
		super();
		this.setSelectionHandler(new SelectionHandler());
	}

	protected InputSelect(InputSelect<T> source) {
		super(source);
		this.setSelectionHandler(new SelectionHandler());
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputSelect<T>(this);
	}

	@Override
	public void onBrowserEvent(Event event) {
		super.onBrowserEvent(event);
		boolean mustKillEvent = false;
		switch (DOM.eventGetType(event)) {
			case Event.ONKEYDOWN:
				switch (event.getKeyCode()) {
					case KeyCodes.KEY_TAB:
					case KeyCodes.KEY_ESCAPE:
						this.getDropdown().close();
						break;
					default:
						break;
				}
				break;
			default:
				break;
		}
		if (mustKillEvent) {
			event.preventDefault();
			event.stopPropagation();
		}
	}
}
