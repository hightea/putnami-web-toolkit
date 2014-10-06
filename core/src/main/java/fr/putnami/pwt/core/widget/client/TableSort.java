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

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.util.PathUtils;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.util.ModelUtils;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.Button.Type;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumnAspect;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

public class TableSort<T> extends AbstractTableColumnAspect<T> implements HasDrawable {


	private final Button<T> button = new Button<T>();

	private Boolean asc;
	private HandlerRegistration buttonRegistration;

	public TableSort() {
		endConstruct();
	}

	protected TableSort(TableSort<T> source) {
		super(source);
		endConstruct();
	}

	private void endConstruct() {
		button.setIconType(IconFont.ICON_SORT);
		button.setType(Type.ICON);
		button.setSize(Button.Size.SMALL);
	}

	@Override
	public AbstractTableColumnAspect<T> cloneAspect() {
		return new TableSort<T>(this);
	}

	@Override
	public Widget asWidget() {
		if (buttonRegistration == null) {
			buttonRegistration = button.addButtonHandler(new ButtonEvent.Handler() {

				@Override
				public void onButtonAction(ButtonEvent event) {
					toggleSort();
				}
			});
		}
		return button;
	}

	@Override
	public void redraw() {
		if (Boolean.TRUE.equals(asc)) {
			button.setIconType(IconFont.ICON_SORT_DOWN);
		}
		else if (Boolean.FALSE.equals(asc)) {
			button.setIconType(IconFont.ICON_SORT_UP);
		}
		else {
			button.setIconType(IconFont.ICON_SORT);
		}
		button.setActive(asc != null);
	}

	private void toggleSort() {
		if (asc == null) {
			asc = Boolean.TRUE;
		}
		else if (Boolean.TRUE.equals(asc)) {
			asc = Boolean.FALSE;
		}
		else {
			asc = null;
		}
		getDriver().resetDisplay();
		redraw();
	}

	@Override
	public <A, B extends Editor> boolean beforeVisit() {
		if (asc != null) {
			ModelDriver<List<T>> localDriver = getDriver();
			final Model<T> leafModel = localDriver.getModel().getLeafModel();
			List<T> list = localDriver.getDisplayedValue();
			list = Lists.newArrayList(list);
			final Path path = PathUtils.evalPath(getColumnPath());
			Comparator<? super T> comparator = new Comparator<T>() {
				@Override
				public int compare(T o1, T o2) {
					Object p1 = ModelUtils.resolveValue(o1, leafModel, path);
					Object p2 = ModelUtils.resolveValue(o2, leafModel, path);

					int result = 0;
					if (p1 instanceof Comparable) {
						result = ((Comparable) p1).compareTo(p2);
					}
					else if (p2 instanceof Comparable) {
						result = ((Comparable) p2).compareTo(p1);
					}
					if (asc == false) {
						result = 0 - result;
					}
					return result;
				}
			};
			Collections.sort(list, comparator);
			localDriver.setDisplayedValue(list);
		}
		return true;
	}

	@Override
	public int getPrecedence() {
		return PRECEDENCE_NORMAL + 2;
	}

}
