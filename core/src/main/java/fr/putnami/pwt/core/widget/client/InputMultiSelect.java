/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.text.shared.AbstractRenderer;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractInputSelect;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class InputMultiSelect<T> extends AbstractInputSelect<T, List<T>> {

	private static final CssStyle STYLE_SELECTED = new SimpleStyle("input-select-selected");
	private static final CssStyle STYLE_HIGHLIGHTED = new SimpleStyle("input-select-highlighted");

	private Comparator<T> simpleComparator = new Comparator<T>() {
		@Override
		public int compare(T o1, T o2) {
			return Integer.compare(InputMultiSelect.this.getOrderedItems().indexOf(o1), InputMultiSelect.this.getOrderedItems().indexOf(o2));
		};
	};

	private class MultiSelectionRenderer extends AbstractRenderer<List<T>> {

		@Override
		public String render(List<T> values) {
			String result = "";
			if (values != null) {
				Collections.sort(values, simpleComparator);
				result = Joiner.on(", ").join(Iterables.transform(values, new Function<T, String>() {
					@Override
					public String apply(T input) {
						return InputMultiSelect.this.getChoiceRenderer().renderItem(input);
					}
				}));
			}
			return result;
		};
	}

	private class MultiSelectionHandler implements SelectItemSelectionHandler<T, List<T>> {
		private int currentHighlightedIndex = -1;
		private List<T> selectedItems;

		@Override
		public void onItemClick(T item) {
			if (selectedItems == null) {
				selectedItems = new ArrayList<T>();
			}
			boolean selectItem = !selectedItems.contains(item);
			selectItem(item, selectItem);
			if (selectItem) {
				selectedItems.add(item);
			}
			else {
				selectedItems.remove(item);
				if (selectedItems.isEmpty()) {
					selectedItems = null;
				}
			}
			highlightByIndex(InputMultiSelect.this.getOrderedItems().indexOf(item));
			ValueChangeEvent.fire(InputMultiSelect.this, selectedItems);
		}

		@Override
		public void onHomeKeyDown() {
			highlightByIndex(0);
		}

		@Override
		public void onEndKeyDown() {
			highlightByIndex(InputMultiSelect.this.getOrderedItems().size() - 1);
		}

		@Override
		public void onUpKeyDown() {
			highlightByIndex(currentHighlightedIndex - 1);
		}

		@Override
		public void onDownKeyDown() {
			highlightByIndex(currentHighlightedIndex + 1);
		}

		private void highlightByIndex(int index) {
			int newIndex = Math.min(InputMultiSelect.this.getOrderedItems().size() - 1, Math.max(0, index));
			highlightItem(currentHighlightedIndex, false);
			highlightItem(newIndex, true);
			currentHighlightedIndex = newIndex;
			scrollToHighlighted();
		}

		private void highlightItem(int itemIndex, boolean highlighted) {
			if (itemIndex != -1) {
				T item = InputMultiSelect.this.getOrderedItems().get(itemIndex);
				NavLink currentSelection = InputMultiSelect.this.getItemsLinks().get(item);
				if (currentSelection != null) {
					StyleUtils.toggleStyle(currentSelection, STYLE_HIGHLIGHTED, highlighted);
				}
			}
		}

		public void onEnterKeyDown() {
			if (currentHighlightedIndex != -1) {
				onItemClick(InputMultiSelect.this.getOrderedItems().get(currentHighlightedIndex));
			}
		}

		@Override
		public void onItemSearch(T searchResult) {
			highlightByIndex(InputMultiSelect.this.getOrderedItems().indexOf(searchResult));
		}

		@Override
		public void setSelection(List<T> selection, boolean fireEvents) {
			List<T> oldSelection = selectedItems;
			if (oldSelection != null) {
				for (T item : oldSelection) {
					selectItem(item, false);
				}
			}

			selectedItems = selection;
			if (selection != null) {
				for (T item : selection) {
					selectItem(item, true);
				}
			}
			highlightItem(currentHighlightedIndex, false);
			currentHighlightedIndex = -1;
			if (fireEvents) {
				ValueChangeEvent.fireIfNotEqual(InputMultiSelect.this, oldSelection, selection);
			}
		}

		private void scrollToHighlighted() {
			if (InputMultiSelect.this.getDropdown().isOpen() && currentHighlightedIndex != -1) {
				T item = InputMultiSelect.this.getOrderedItems().get(currentHighlightedIndex);
				NavLink currentSelection = InputMultiSelect.this.getItemsLinks().get(item);
				if (currentSelection != null) {
					currentSelection.getElement().scrollIntoView();
				}
			}
		}

		private void selectItem(T item, boolean selected) {
			NavLink currentSelection = InputMultiSelect.this.getItemsLinks().get(item);
			if (currentSelection != null) {
				currentSelection.setActive(selected);
				StyleUtils.toggleStyle(currentSelection, STYLE_SELECTED, selected);
			}
		}

		@Override
		public List<T> getSelection() {
			return selectedItems;
		}
	}

	public InputMultiSelect() {
		super();
		setMultiple(true);
		setSelectionHandler(new MultiSelectionHandler());
		setSelectionRenderer(new MultiSelectionRenderer());
	}

	protected InputMultiSelect(InputMultiSelect<T> source) {
		super(source);
		setSelectionHandler(new MultiSelectionHandler());
		setSelectionRenderer(new MultiSelectionRenderer());
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputMultiSelect<T>(this);
	}

	@Override
	public void addItem(T item) {
		super.addItem(item);
		getItemsLinks().get(item).setPreventClickWhenActive(false);
	}

	@Override
	public void onBrowserEvent(Event event) {
		super.onBrowserEvent(event);
		boolean mustKillEvent = false;
		switch (DOM.eventGetType(event)) {
		case Event.ONKEYDOWN:
			switch (event.getKeyCode()) {
			case KeyCodes.KEY_ENTER:
				if (getDropdown().isOpen()) {
					((MultiSelectionHandler) getSelectionHandler()).onEnterKeyDown();
					mustKillEvent = true;
				}
				break;
			case KeyCodes.KEY_TAB:
			case KeyCodes.KEY_ESCAPE:
				getDropdown().close();
				break;
			}
			break;
		}
		if (mustKillEvent) {
			event.preventDefault();
			event.stopPropagation();
		}
	}

}
