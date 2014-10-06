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
package fr.putnami.pwt.core.widget.incubating;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.List;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.util.PathUtils;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.util.ModelUtils;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.Button;
import fr.putnami.pwt.core.widget.client.Button.Type;
import fr.putnami.pwt.core.widget.client.Container;
import fr.putnami.pwt.core.widget.client.InputText;
import fr.putnami.pwt.core.widget.client.TableTH;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumnAspect;
import fr.putnami.pwt.core.widget.client.base.HasHeaderCell;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

public class TableFilter<T> extends AbstractTableColumnAspect<T> implements HasHeaderCell {

	private final Button<T> button = new Button<T>();

	private Boolean activate = false;

	private String filterValue;
	private final InputText inputText = new InputText();
	private TableTH<?> headerCell;

	private HandlerRegistration valueChangeRegistration;

	public TableFilter() {
		this.endConstruct();
	}

	protected TableFilter(TableFilter<T> source) {
		super(source);
		this.endConstruct();
	}

	private void endConstruct() {
		this.button.setIconType(IconFont.ICON_FILTER);
		this.button.setType(Type.ICON);
		this.button.setSize(Button.Size.SMALL);

		this.button.addButtonHandler(new ButtonEvent.Handler() {
			@Override
			public void onButtonAction(ButtonEvent event) {
				TableFilter.this.toggle();
			}
		});
	}

	@Override
	public AbstractTableColumnAspect<T> cloneAspect() {
		return new TableFilter<T>(this);
	}

	@Override
	public Widget asWidget() {
		return this.button;
	}

	@Override
	public void setHeaderCell(TableTH<?> headerCell) {
		this.headerCell = headerCell;
	}

	private void toggle() {
		this.activate = !this.activate;
		this.redraw();
		this.getDriver().resetDisplay();
	}

	public void redraw() {
		this.button.setIconType(IconFont.ICON_FILTER);
		this.button.setActive(this.activate);

		Container row = (Container) ((Widget) this.headerCell).getParent();

		this.headerCell.clear();
		if (this.valueChangeRegistration != null) {
			this.valueChangeRegistration.removeHandler();
			this.valueChangeRegistration = null;
		}
		boolean showFilterRow = false;
		if (this.activate) {
			this.headerCell.append(this.inputText);
			this.valueChangeRegistration = this.inputText.addBlurHandler(new BlurHandler() {
				@Override
				public void onBlur(BlurEvent event) {
					String newValue = TableFilter.this.inputText.flush();
					if (!Objects.equal(TableFilter.this.filterValue, newValue)) {
						TableFilter.this.filterValue = newValue;
						TableFilter.this.getDriver().resetDisplay();
					}
				}
			});

			Widget hCell = this.button.getParent().getParent();
			hCell.getElement().getStyle().setWidth(hCell.getOffsetWidth(), Unit.PX);

			showFilterRow = true;
		}
		if (!showFilterRow) {
			for (Widget cell : row) {
				if (cell instanceof TableTH) {
					showFilterRow |= Iterators.size(((TableTH) cell).iterator()) > 0;
					if (showFilterRow) {
						break;
					}
				}
			}
		}
		row.setVisible(showFilterRow);
	}

	@Override
	public int getPrecedence() {
		return Visitor.PRECEDENCE_NORMAL + 3;
	}

	@Override
	public <A, B extends Editor> boolean beforeVisit() {
		if (this.activate && this.filterValue != null) {
			final Model<T> leafModel = this.getDriver().getModel().getLeafModel();
			final Path path = PathUtils.evalPath(this.getColumnPath());

			List<T> list = this.getDriver().getDisplayedValue();
			list = Lists.newArrayList(Iterables.filter(list, new Predicate<T>() {

				@Override
				public boolean apply(T input) {
					Object p1 = ModelUtils.resolveValue(input, leafModel, path);
					if (p1 instanceof String) {
						return ((String) p1).toLowerCase().contains(TableFilter.this.filterValue.toLowerCase());
					}
					return true;
				}
			}));
			this.getDriver().setDisplayedValue(list);
		}
		return true;
	}
}
