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
package fr.putnami.pwt.core.widget.client.helper;

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.HasDriver;
import fr.putnami.pwt.core.widget.client.Pagination;
import fr.putnami.pwt.core.widget.client.base.AbstractComposite;
import fr.putnami.pwt.core.widget.client.event.PageChangeEvent;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class PaginationHelper<T> extends AbstractComposite implements
		PageChangeEvent.Handler,
		Editor,
		HasDriver<Collection<T>,
		ModelDriver<Collection<T>>>,
		Visitor {

	private final Pagination pagination;

	private String path;
	private ModelDriver<Collection<T>> driver;

	public PaginationHelper(Pagination pagination) {
		this.pagination = pagination;
		endConstruct();
	}

	protected PaginationHelper(PaginationHelper<T> source) {
		super(source);

		this.path = source.path;
		this.pagination = WidgetUtils.cloneWidget(source.pagination);
		endConstruct();
	}

	private void endConstruct() {
		initWidget(this.pagination);
		pagination.addPageChangeHandler(this);
	}

	@Override
	public IsWidget cloneWidget() {
		return new PaginationHelper<T>(this);
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.BEFORE_EDIT;
	}

	@Override
	public <A, B extends Editor> boolean beforeVisit() {
		Collection<T> list = driver.getDisplayedValue();
		int nbItems = list.size();
		int fromIndex = pagination.getPageSize() * pagination.getCurrentPage();
		while (fromIndex > nbItems) {
			fromIndex -= pagination.getPageSize();
		}
		int toIndex = fromIndex + pagination.getPageSize();
		if (toIndex + 1 > nbItems) {
			toIndex = nbItems;
		}
		int nbPage = (int) Math.ceil((0d + nbItems) / pagination.getPageSize());
		list = Lists.newArrayList(list).subList(fromIndex, toIndex);
		driver.setDisplayedValue(Lists.newArrayList(list));

		pagination.setNbPage(nbPage);
		return true;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		return true;
	}

	@Override
	public <A, B extends Editor> boolean afterVisit() {
		return true;
	}

	@Override
	public void onPageChange(PageChangeEvent event) {
		driver.resetDisplay();
	}

	@Override
	public ModelDriver<Collection<T>> getDriver() {
		return driver;
	}

	@Override
	public void setDriver(ModelDriver<Collection<T>> driver) {
		this.driver = driver;
	}

	@Override
	public int getPrecedence() {
		return PRECEDENCE_NORMAL + 1;
	}

	@Override
	public String getPath() {
		return path == null ? Path.ROOT_PATH : path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

}
